#include "racket_binder.h"

#include "scheme_script.h"
#include "scheme_script_instance.h"
#include "util/scheme_util.h"
#include "godot_cpp/classes/engine.hpp"

#include <stdio.h>
#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>

#include "./racket_binder_util.h"
#include "./racket_gdprimitive.h"
#include "./racket_builtin_binder.h"

// Include Racket bytecode-bootstrapping sources
// racoBC ctool --c-mods racket_modules.c ++lib racket/main ++lib racket/base ++lib racket/class ++lib racket/vector ++lib racket/lang/reader ++lib racket/runtime-config
#include "racket_modules.c"


void _log(std::string msg) {
    std::cout << msg;
}


void _setup_tls_space() {
#ifdef _WIN32
    // TODO: non-windows version of this if needed
    static __declspec(thread) void *tls_space;
    scheme_register_tls_space(tls_space, 0);
#endif
}


// TODO: figure out if it's at all possible to instantiate Scheme_Env in heap
// (unlikely since BC racket seems to put all its main stuff in statics :cry:)
// this will need significant rework to make multi-threaded
static Scheme_Env* root_scheme_env;
static Scheme_Object* root_env_output;

#define RKT_TO_HASH(sch_obj) SCHEME_HASHTRP(sch_obj) ? (Scheme_Hash_Tree*)sch_obj : nullptr;
#define RKT_HASH_GET(so, key, deflt) rkt_eval(rkt_sym("hash-ref"), so, rkt_quote(key), deflt)

#define SYM_NAME rkt_sym("name")
#define SYM_TYPE rkt_sym("type")
#define SYM_PROP rkt_sym("prop")
#define SYM_RETURN rkt_sym("return")
#define SYM_ARGS rkt_sym("args")

#define SYM_FUNC rkt_sym("func")
#define SYM_VAR rkt_sym("var")
#define SYM_EXPORT_VAR rkt_sym("export-var")
#define SYM_SIGNAL rkt_sym("signal")

#define RKT_EMPTY_LIST rkt_quote(scheme_null)
#define ERR_FAIL_ERR(expr) { Error st = expr; ERR_FAIL_COND_V(st != Error::OK, st); }


// TODO: figure out a better way to do all of these class metadata-extracting functions, tend to be brittle
_FORCE_INLINE_ Error hash_to_gd_prop(Scheme_Object* p_obj, GDProperty& prop) {
    ERR_FAIL_COND_V_MSG(scheme_false == rkt_eval(rkt_sym("hash?"), p_obj), Error::FAILED, "not a hash table");
    prop.name = rktsym2gdstrname(RKT_HASH_GET(p_obj, SYM_NAME, rkt_quote_sym("error")));
    prop.type = (GDExtensionVariantType)SCHEME_INT_VAL(RKT_HASH_GET(p_obj, SYM_TYPE, rkt_int(0)));
    return Error::OK;
}


_FORCE_INLINE_ Error hash_to_gd_class_prop(Scheme_Object* p_obj, GDClassProperty& cprop) {
    ERR_FAIL_COND_V_MSG(scheme_false == (rkt_sym("hash?"), p_obj), Error::FAILED, "not a hash table");
    cprop.symbol = rktsym2gdstrname(RKT_HASH_GET(p_obj, SYM_NAME, rkt_quote_sym("error")));
    ERR_FAIL_ERR(hash_to_gd_prop(RKT_HASH_GET(p_obj, SYM_PROP, rkt_eval_string("#hash()")), cprop.property));
    cprop.property.name = cprop.symbol.replace("-", "_");
    return Error::OK;
}


_FORCE_INLINE_ Error hash_to_gd_method(Scheme_Object* p_obj, GDMethod& mthd) {
    ERR_FAIL_COND_V_MSG(scheme_false == (rkt_sym("hash?"), p_obj), Error::FAILED, "not a hash table");
    mthd.symbol = rktsym2gdstrname(RKT_HASH_GET(p_obj, SYM_NAME, rkt_quote_sym("error")));
    mthd.name = mthd.symbol.replace("-", "_");
    mthd.return_val.type = (GDExtensionVariantType)SCHEME_INT_VAL(RKT_HASH_GET(p_obj, SYM_RETURN, rkt_int(0)));

    Scheme_Object* mthd_args = RKT_HASH_GET(p_obj, SYM_ARGS, RKT_EMPTY_LIST);
    while (mthd_args != scheme_null) {
        GDProperty prop;
        Error st = hash_to_gd_prop(SCHEME_CAR(mthd_args), prop);
        if (st != Error::OK) {
            return st;
        }
        mthd.arguments.append(prop);
        mthd_args = SCHEME_CDR(mthd_args);
    }
    return Error::OK;
}


Error hash_to_class_definition(Scheme_Object* p_obj, GDClassDefinition& class_def) {
    ERR_FAIL_COND_V_MSG(scheme_false == rkt_eval(rkt_sym("hash?"), p_obj), Error::FAILED, "not a hash table");
    class_def.name = rktsym2gdstrname(RKT_HASH_GET(p_obj, SYM_NAME, rkt_quote_sym("error")));
    Scheme_Object* vars = RKT_HASH_GET(p_obj, SYM_VAR, RKT_EMPTY_LIST);
    Scheme_Object* export_vars = RKT_HASH_GET(p_obj, SYM_EXPORT_VAR, RKT_EMPTY_LIST);
    Scheme_Object* methods = RKT_HASH_GET(p_obj, SYM_FUNC, RKT_EMPTY_LIST);
    Scheme_Object* signals = RKT_HASH_GET(p_obj, SYM_SIGNAL, RKT_EMPTY_LIST);

    Scheme_Object* all_vars[] = {
        RKT_HASH_GET(p_obj, SYM_VAR, RKT_EMPTY_LIST),
        RKT_HASH_GET(p_obj, SYM_EXPORT_VAR, RKT_EMPTY_LIST)
    };

    while (vars != scheme_null) {
        GDClassProperty cprop;
        cprop.usage = (PropertyUsageFlags)
            (cprop.usage & ~PropertyUsageFlags::PROPERTY_USAGE_DEFAULT);
        ERR_FAIL_ERR(hash_to_gd_class_prop(SCHEME_CAR(vars), cprop));
        class_def.register_prop(cprop.property.name, cprop);
        vars = SCHEME_CDR(vars);
    }

    while (export_vars != scheme_null) {
        GDClassProperty cprop;
        ERR_FAIL_ERR(hash_to_gd_class_prop(SCHEME_CAR(export_vars), cprop));
        cprop.usage = (PropertyUsageFlags)
            (cprop.usage | PropertyUsageFlags::PROPERTY_USAGE_DEFAULT);
        class_def.register_prop(cprop.property.name, cprop);
        export_vars = SCHEME_CDR(export_vars);
    }

    while (methods != scheme_null) {
        GDMethod mthd;
        ERR_FAIL_ERR(hash_to_gd_method(SCHEME_CAR(methods), mthd));
        class_def.register_method(mthd.name, mthd);
        methods = SCHEME_CDR(methods);
    }
    return Error::OK;
}


// Environment setup ///////////////////////////////////////////////////////////

void _env_setup_globals(Scheme_Env* env, Scheme_Object* collects_path_string_list) {
    gdprimitive_init_module(env);

    declare_modules(env);

    scheme_namespace_require(scheme_intern_symbol("racket/base"));
    scheme_namespace_require(scheme_intern_symbol("racket/exn"));
    scheme_namespace_require(scheme_intern_symbol("racket/class"));
    scheme_namespace_require(scheme_intern_symbol("racket/vector"));

    // // TODO: create bytecode embed for system modules
    scheme_init_collection_paths(env, collects_path_string_list);
    scheme_namespace_require(rkt_sym("gdracket/mod"));
    scheme_namespace_require(rkt_sym("gdracket/godot"));
}


// TODO: figure out how to do this better
Scheme_Object* _env_fork_namespace(const char* ns_name, Scheme_Env* parent_env) {
    Scheme_Object* src_ns = scheme_eval_string("(current-namespace)", parent_env);
    Scheme_Object* dest_ns = scheme_eval_string("(make-base-namespace)", parent_env);

    scheme_eval(rkt_list(
        rkt_sym("attach-modules"),
        src_ns,
        dest_ns,
        rkt_quote(
            rkt_list(
                rkt_sym("racket/base"),
                rkt_sym("racket/class"),
                rkt_sym("racket/vector"),
                rkt_sym("gdracket/mod"),
                rkt_sym("gdracket/godot")
                ))
    ), parent_env);

    return dest_ns;
}


RacketBinder::RacketBinder() {
    _setup_tls_space();
    scheme_main_stack_setup(1, [](void* data) -> int {
        RacketBinder* setup_data = (RacketBinder*)data;

        Scheme_Object* collects_paths = rkt_list(
            gdstr2rktstr(SchemeLanguage::get_singleton()->get_install_dir()));

        root_scheme_env = scheme_basic_env();
        // // TODO: Create port that pipes racket output to godot's
        root_env_output = scheme_make_file_output_port(stdout);
        scheme_set_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT, root_env_output);

        BuiltinBinder::get_singleton()->register_builtin_types();
        _env_setup_globals(root_scheme_env, collects_paths);

        return 0;
    }, (void*)this);
}


RacketBinder::~RacketBinder() {
    // TODO: figure out how to tear down Racket
}


GDClassDefinition* RacketBinder::get_definition(const SchemeScript &script) {
    if (script_id_definition_map.has(script.get_instance_id())) {
        return &script_id_definition_map.get(script.get_instance_id());
    }
    return nullptr;
}


// TODO: finalize definitions when scripts are changed or deleted
Error RacketBinder::create_definition(const SchemeScript &script, GDClassDefinition& def) {
    // GDClassDefinition gdcd;

    const char* script_source = script.get_source_code().utf8().get_data();
    const char* script_name = script.get_path().utf8().get_data();
    Scheme_Object* script_module_name = rkt_sym(script_name);

    Scheme_Object* script_ns = scheme_eval(rkt_list(rkt_sym("current-namespace")), root_scheme_env);

    // Evaluate script as a module and get a hash table populated with script info
    Scheme_Object* result = rkt_eval(
        rkt_sym("eval-handle"),
        rkt_list(
            rkt_sym("register-gd-class"),
            scheme_make_utf8_string(script_source),
            rkt_quote(script_module_name),
            script_ns)
        );

    Scheme_Object* ok = SCHEME_CAR(result);
    if (SCHEME_FALSEP(ok)) {
        rkt_eval(rkt_sym("push-exn"), SCHEME_CADR(result));
        return Error::ERR_INVALID_DECLARATION;
    }

    result = SCHEME_CDR(result);
    hash_to_class_definition(result, def);

    script_id_class_map.insert(script.get_instance_id(), script_module_name);
    script_id_definition_map.insert(script.get_instance_id(), def);
    return Error::OK;
}


void RacketBinder::delete_definition(const SchemeScript &p_script) {
    uint32_t instance_id = p_script.get_instance_id();
    script_id_class_map.erase(instance_id);
    script_id_definition_map.erase(instance_id);
}


Error RacketBinder::initialize_instance(SchemeScriptInstance &p_target) {
    // TODO: allow non-class instances
    uint64_t script_instance_id = p_target.get_script()->get_instance_id();
    if (!script_id_class_map.has(script_instance_id)) {
        rkt_eval(
            rkt_sym("push-error"),
            rkt_string("Error creating instance"));
        return Error::ERR_UNAVAILABLE;
    }
    uint32_t inst_id = p_target.get_owner()->get_instance_id();
    Scheme_Object* class_name_symbol = script_id_class_map.get(p_target.get_script()->get_instance_id());
    Scheme_Object* script_ns = scheme_eval(rkt_list(rkt_sym("current-namespace")), root_scheme_env);

    Scheme_Object* instance = scheme_eval(
        rkt_list(
            rkt_sym("new-gd-class-instance"),
            rkt_quote(class_name_symbol),
            script_ns
        ), root_scheme_env);

    if (SCHEME_FALSEP(SCHEME_CAR(instance))) {
        rkt_eval(
            rkt_sym("push-exn"),
            SCHEME_CDR(instance),
            rkt_string("Error creating instance"));
        return Error::ERR_SCRIPT_FAILED;
    } else {
        Scheme_Object* cls_instance = SCHEME_CDR(instance);
        // CRITICAL: Prevent Racket GC on instances until Godot frees the Object
        // this script is attached to
        scheme_dont_gc_ptr(cls_instance);
        instance_id_ctx_map.insert(inst_id, cls_instance);
    }
    return Error::OK;
}


int32_t RacketBinder::get_member_line(SchemeScriptInstance &p_target, const StringName &member) {
    return 0;
}


Variant RacketBinder::call(SchemeScriptInstance &p_target, const String p_func_name, const Variant **p_args, int p_argcount, SchemeCallError* r_error) {
    uint32_t instance_id = p_target.get_owner()->get_instance_id();
    uint32_t script_id = p_target.get_script()->get_instance_id();

    ERR_FAIL_COND_V_MSG(!instance_id_ctx_map.has(instance_id), Variant(), "No script instance found for instance " + p_target.get_owner()->to_string());

    Scheme_Object* instance = instance_id_ctx_map.get(instance_id);
    GDClassDefinition* def = &script_id_definition_map.get(script_id);

    ERR_FAIL_COND_V_MSG(!def->has_method(p_func_name), Variant(), "Instance has no method  " + p_func_name);

    Scheme_Object* args_list = scheme_null;
    if (p_argcount > 0) {
        Scheme_Object** objs = (Scheme_Object**)malloc(sizeof(Scheme_Object*) * p_argcount);
        for (int i = 0; i < p_argcount; i++) {
            const Variant* arg_i = p_args[i];
            objs[i] = gd_obj2rkt_obj(arg_i);
        }
        args_list = scheme_build_list(p_argcount, objs);
    }

    Scheme_Object* method_name_sym = rkt_sym(gdstr2charp(((const String)def->get_method(p_func_name)->symbol)));

    Scheme_Object* out = rkt_eval_handle(
        rkt_sym("invoke"),
        instance,
        rkt_quote(method_name_sym),
        rkt_quote(args_list));

    if (SCHEME_FALSEP(SCHEME_CAR(out))) {
        rkt_eval(rkt_sym("push-exn"), SCHEME_CDR(out));
        return Variant();
    }
    Scheme_Object* cadr = SCHEME_CDR(out);
    return rkt_obj2gd_obj(cadr);
}


bool RacketBinder::set(SchemeScriptInstance &p_target, const StringName &p_name, const Variant &p_value) {
    // TODO: set up something to reduce hash table lookups?
    uint32_t owner_id = p_target.get_owner()->get_instance_id();
    uint32_t script_id = p_target.get_script()->get_instance_id();

    ERR_FAIL_COND_V_MSG(!instance_id_ctx_map.has(owner_id) || !script_id_definition_map.has(script_id), false, "No script instance found for object " + p_target.get_owner()->to_string());

    GDClassDefinition* def = &script_id_definition_map.get(script_id);
    if (!def->has_prop(p_name)) {
        return false;
    }
    Scheme_Object* instance = instance_id_ctx_map.get(owner_id);
    Scheme_Object* prop_symbol = rkt_sym((gdstr2charp(((const String)def->get_prop(p_name)->symbol))));
    Scheme_Object* wrapped = BuiltinBinder::get_singleton()->variant_to_scheme_object(p_value);
    rkt_eval(rkt_sym("set-field!"), prop_symbol, instance, wrapped);
    return true;
}


bool RacketBinder::get(const SchemeScriptInstance &p_target, const StringName p_name, Variant *r_ret) const {
    uint32_t owner_id = p_target.get_owner()->get_instance_id();
    uint32_t script_id = p_target.get_script()->get_instance_id();

    ERR_FAIL_COND_V_MSG(!instance_id_ctx_map.has(owner_id) || !script_id_definition_map.has(script_id), false, "No script instance found for object " + p_target.get_owner()->to_string());

    const GDClassDefinition* def = &script_id_definition_map.get(script_id);
    ERR_FAIL_COND_V_MSG(!def->has_prop(p_name), false, "Class " + def->name + "Does not have property " + p_name);

    Scheme_Object* instance = instance_id_ctx_map.get(owner_id);
    Scheme_Object* prop_symbol = rkt_sym((gdstr2charp(((const String)def->get_prop(p_name)->symbol))));

    Scheme_Object* res = rkt_eval(rkt_sym("get-field"), prop_symbol, instance);

    *r_ret = BuiltinBinder::get_singleton()->scheme_object_to_variant(res);
    return true;
}


bool RacketBinder::has_method(const SchemeScriptInstance &p_target, const StringName &p_method) const {
    uint32_t scr_instid = p_target.get_script()->get_instance_id();
    if (!script_id_definition_map.has(scr_instid)) {
        return false;
    }
    const GDClassDefinition* defn = &(script_id_definition_map.get(scr_instid));
    return defn->has_method(p_method);
}


void RacketBinder::free_instance(SchemeScriptInstance &p_target) {
    uint32_t inst_id = p_target.get_owner()->get_instance_id();
    if (!instance_id_ctx_map.has(inst_id)) {
        return;
    }
    Scheme_Object* cls_instance = instance_id_ctx_map.get(inst_id);
    // CRITICAL: allow Racket to garbage-collect instance now
    scheme_gc_ptr_ok(cls_instance);
    instance_id_ctx_map.erase(inst_id);
}
