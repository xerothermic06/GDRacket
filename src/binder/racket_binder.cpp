// This should be the only source file that touches any part of RacketBC

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

// Include Racket bytecode-bootstrapping sources
// racoBC ctool --c-mods /d/projects/godot/gdextension-helloworld2/src/binder/racket_modules.c ++lib racket/main ++lib racket/base ++lib racket/class ++lib racket/vector ++lib racket/lang/reader ++lib racket/runtime-config
// racoBC ctool --c-mods racket_modules.c ++lib racket/base ++lib racket/class
#include "racket_modules.c"

// Racket shortcuts

// Builds a list of Racket objects from varargs
template<typename... T>
Scheme_Object* rkt_list(T... ts) {
    const int size = sizeof...(ts);
    static_assert(size > 0);
    Scheme_Object* rest[size] = {ts...};
    Scheme_Object* list = scheme_build_list(size, rest);
    return list;
}

// Get interned symbol
#define rkt_sym(x) scheme_intern_symbol(x)
// Quote a Scheme_Object*
#define rkt_quote(x) rkt_list(rkt_sym("quote"), x)
// Create an (obj . obj) pair
#define rkt_pair(x, y) scheme_make_pair(x, y)
#define rkt_keyword(x) scheme_intern_exact_keyword(x, sizeof(x) - 1)
#define rkt_string(x) scheme_make_utf8_string(x)
// Make a Racket UTF-8 string from a Godot string
#define gdstr2rktstr(x) scheme_make_utf8_string(x.utf8().get_data())
#define gdstrname2rktstr(x) scheme_make_utf8_string(String(x).utf8().get_data())
#define gdstr2charp(x) (x.utf8().get_data())

// TODO: figure out how to make this work if some platform has an `unsigned int`
// that isn't exactly a uint32_t
#define rktstr2gdstr(x) String((const char32_t*)SCHEME_CHAR_STR_VAL(x))
#define rktstr2gdstrname(x) StringName((const char32_t*)SCHEME_CHAR_STR_VAL(x))
#define rkt_eval(...) scheme_eval(rkt_list(__VA_ARGS__), root_scheme_env)
#define rkt_eval_handle(...) scheme_eval(rkt_list(rkt_sym(EVAL_HANDLE), rkt_quote(rkt_list(__VA_ARGS__))), root_scheme_env)


static const char* VECTOR_MAP_BREAKABLE = "vector-map-breakable";
static const char* READ_MULTI = "read-multi";
static const char* EVAL_HANDLE = "eval-handle";
static const char* EVAL_SOURCE = "eval-source";
static const char* RACKET_GDVARIANT_TYPE_NAME = "gd-variant";


void _log(std::string msg) {
    std::cout << msg;
}

void _logln(std::string msg) {
    std::cout << msg << std::endl;
}

void _setup_tls_space() {
#ifdef _WIN32
    // TODO: non-windows version of this if needed
    static __declspec(thread) void *tls_space;
    scheme_register_tls_space(tls_space, 0);
#endif
}


Scheme_Object* gd_obj2rkt_obj(const Variant* v) {
    switch (v->get_type()) {
        case Variant::BOOL:
            return (bool)v ? scheme_true : scheme_false;
        case Variant::INT:
            return scheme_make_integer((uint32_t)*v);
        case Variant::FLOAT:
            return scheme_make_float((float)*v);
        case Variant::STRING:
            return gdstr2rktstr(((String)*v));
        case Variant::STRING_NAME:
            return gdstrname2rktstr(((StringName)*v));
        case Variant::OBJECT:
            return scheme_make_external_cptr((void*)(Object*)*v, rkt_sym("object"));
    }
    return scheme_void;
}


Variant rkt_obj2gd_obj(Scheme_Object* obj) {
    if (SCHEME_BOOLP(obj)) { return Variant(SCHEME_TRUEP(obj)); }
    if (SCHEME_INTP(obj)) { return Variant(SCHEME_INT_VAL(obj)); }
    if (SCHEME_FLOATP(obj)) { return Variant(SCHEME_FLOAT_VAL(obj)); }
    if (SCHEME_CHAR_STRINGP(obj)) { return Variant(rktstr2gdstr(obj)); }
    if (SCHEME_CHAR_STRINGP(obj)) { return Variant(rktstr2gdstr(obj)); }
    if (SCHEME_CPTRP(obj)) {
        if (SCHEME_CPTR_TYPE(obj) == rkt_sym("object")) {
            return Variant((Object*)SCHEME_CPTR_VAL(obj));
        }
    }
    return Variant();
}


Scheme_Object* rkt_make_gd_string(int argc, Scheme_Object** argv) {
    if (argc < 1) {
        return scheme_void;
    }
    String* str = memnew(rktstr2gdstr(argv[0]));
    return scheme_make_cptr(str, rkt_sym("String"));
}


Scheme_Object* rkt_make_gd_stringname(int argc, Scheme_Object** argv) {
    if (argc < 1) {
        return scheme_void;
    }
    StringName* str = memnew(rktstr2gdstrname(argv[0]));
    return scheme_make_cptr(str, rkt_sym("StringName"));
}


static Scheme_Env* root_scheme_env;
static Scheme_Object* root_env_output;


GDClassDefinition convert_gd_class_definition(Scheme_Object* gd_prop_struct_inst) {
    GDClassDefinition class_def;

    Scheme_Object* props = rkt_eval(rkt_sym("gd-class-properties"), gd_prop_struct_inst);
    Scheme_Object* methods = rkt_eval(rkt_sym("gd-class-methods"), gd_prop_struct_inst);

    while (props != scheme_null) {
        GDProperty prop;
        GDClassProperty cprop;
        Scheme_Object* prop_obj = SCHEME_CAR(props);
        prop.name = rktstr2gdstrname(rkt_eval(rkt_sym("gd-prop-name"), prop_obj));
        prop.type = (GDExtensionVariantType)SCHEME_INT_VAL(rkt_eval(rkt_sym("gd-prop-type"), prop_obj));
        cprop.property = prop;
        class_def.set_prop(prop.name, cprop);
        props = SCHEME_CDR(props);
    }

    while (methods != scheme_null) {
        GDMethod mthd;
        Scheme_Object* mthd_obj = SCHEME_CAR(methods);
        mthd.name = rktstr2gdstrname(rkt_eval(rkt_sym("gd-method-name"), mthd_obj));
        Scheme_Object* mthd_args = rkt_eval(rkt_sym("gd-method-arguments"), mthd_obj);
        while (mthd_args != scheme_null) {
            GDProperty prop;
            prop.type = (GDExtensionVariantType)SCHEME_INT_VAL(SCHEME_CAR(mthd_args));
            mthd.arguments.append(prop);
            mthd_args = SCHEME_CDR(mthd_args);
        }
        class_def.set_method(mthd.name, mthd);
        methods = SCHEME_CDR(methods);
    }
    return class_def;
}


// gd-primitive module procedures //////////////////////////////////////////////

Scheme_Object* _gdprimitive_push_error(int argc, Scheme_Object** argv) {
    if (SCHEME_CHAR_STRINGP(argv[0])) {
        UtilityFunctions::push_error(rktstr2gdstr(argv[0]));
    }
    // TODO: Racket exception
    return scheme_void;
}


Scheme_Object* _gdprimitive_get_singleton(int argc, Scheme_Object** argv) {
    if (SCHEME_CHAR_STRINGP(argv[0])) {
        Object* obj = Engine::get_singleton()->get_singleton(rktstr2gdstrname(argv[0]));
        if (obj != nullptr) {
            return gd_obj2rkt_obj(&Variant(obj));
        }
    }
    // TODO: Racket exception
    return scheme_void;
}


Scheme_Object* _gdprimitive_gdobjectp(int argc, Scheme_Object** argv) {
    return (SCHEME_CPTRP(argv[0])) && (scheme_eq(SCHEME_CPTR_TYPE(argv[0]), rkt_sym("object")) == 1)
        ? scheme_true
        : scheme_false;
}


Scheme_Object* _gdprimitive_gd_object_call(int argc, Scheme_Object** argv) {
    if (!SCHEME_CPTRP(argv[0])
        || !(scheme_eq(SCHEME_CPTR_TYPE(argv[0]), rkt_sym("object")) == 1)
        || !(SCHEME_SYMBOLP(argv[1]))) {
        return scheme_void;
    }
    Object* obj = (Object*)SCHEME_CPTR_VAL(argv[0]);
    Scheme_Object* method_symbol = rkt_sym(SCHEME_SYM_VAL(argv[1]));
    Scheme_Object* method_name_str = rkt_eval(rkt_sym("symbol->string"), method_symbol);
    StringName method_name = rktstr2gdstrname(method_name_str);
    Array args;
    for (int i = 0; i < argc - 2; i++) {
        args.append(rkt_obj2gd_obj(argv[i]));
    }
    Variant result = obj->callv(method_name, args);
    return gd_obj2rkt_obj(&result);
}

#define rkt_add_prim_module_proc(nm, fn, modul, minargs, maxargs, returns) \
    scheme_add_global(nm, scheme_make_prim_w_everything(fn, 1, nm, minargs, maxargs, 0, returns, returns), modul)


Scheme_Env* init_gdprimitive_module(Scheme_Env* env) {
    Scheme_Env* modul = scheme_primitive_module(rkt_sym("gd-primitive"), env);
    rkt_add_prim_module_proc("push-error", _gdprimitive_push_error, modul, 1, 1, 0);
    rkt_add_prim_module_proc("get-singleton", _gdprimitive_get_singleton, modul, 1, 1, 1);
    rkt_add_prim_module_proc("object?", _gdprimitive_gdobjectp, modul, 1, 1, 1);
    rkt_add_prim_module_proc("object-call", _gdprimitive_gd_object_call, modul, 2, 16, 1);
    scheme_finish_primitive_module(modul);
    return modul;
}


// Environment setup ///////////////////////////////////////////////////////////

void _env_setup_globals(Scheme_Env* env, Scheme_Object* collects_path_string_list) {
    init_gdprimitive_module(env);

    declare_modules(env);

    scheme_namespace_require(scheme_intern_symbol("racket/base"));
    scheme_namespace_require(scheme_intern_symbol("racket/exn"));
    scheme_namespace_require(scheme_intern_symbol("racket/class"));
    scheme_namespace_require(scheme_intern_symbol("racket/vector"));

    // // TODO: create bytecode embed for system modules
    scheme_init_collection_paths(env, collects_path_string_list);
    scheme_namespace_require(rkt_sym("gdracket/mod"));
    scheme_namespace_require(rkt_sym("gdracket/godot"));
    // scheme_eval(rkt_list(rkt_sym("require"), rkt_sym("gdracket/godot")), env);
    // scheme_eval(rkt_list(rkt_sym("require"), rkt_sym("gdracket/mod")), env);
}

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


RacketEnvironment::RacketEnvironment() {
    sem = new(std::condition_variable);
    mutex = new(std::mutex);
    ready = false;
    setup_done = false;

    _setup_tls_space();

    scheme_main_stack_setup(1, [](void* data) -> int {
        RacketEnvironment* setup_data = (RacketEnvironment*)data;

        Scheme_Object* collects_paths = rkt_list(
            gdstr2rktstr(SchemeLanguage::get_singleton()->get_install_dir()));

        root_scheme_env = scheme_basic_env();
        // // TODO: Create port that pipes racket output to godot's
        root_env_output = scheme_make_file_output_port(stdout);
        scheme_set_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT, root_env_output);
        _env_setup_globals(root_scheme_env, collects_paths);

        Scheme_Env* env = root_scheme_env;
        setup_data->set_env(env);

        return 0;
    }, (void*)this);
}


Scheme_Object* RacketEnvironment::eval_in(Scheme_Object* eval_list, Scheme_Env* ns) {
    request.result = scheme_eval(eval_list, ns);
    return request.result;
}


Scheme_Object* RacketEnvironment::eval(Scheme_Object* eval_list) {
    return eval_in(eval_list, env);
}


Scheme_Object* RacketEnvironment::eval_string_in(std::string eval_str, Scheme_Env* ns) {
    request.result = scheme_eval_string(eval_str.c_str(), ns);
    return request.result;
}


Scheme_Object* RacketEnvironment::eval_string(std::string eval_str) {
    return eval_string_in(eval_str, env);
}


Scheme_Object* RacketEnvironment::eval_func_in(RacketMapper* mapper_func, Scheme_Object* args, Scheme_Env* ns) {
    return request.result;
}


void RacketBinder::initialize() {
    environment = new RacketEnvironment();
}


void RacketBinder::uninitialize() {
}


GDClassDefinition RacketBinder::scheme_create_definition(const SchemeScript &script) {
    GDClassDefinition gdcd;

    const char* script_source = script.get_source_code().utf8().get_data();
    Scheme_Object* script_name = rkt_sym(script.get_name().utf8().get_data());

    script_name = rkt_sym("test-script");
    Scheme_Object* script_ns = scheme_eval(rkt_list(rkt_sym("current-namespace")), root_scheme_env);
    // Scheme_Object* script_ns = _env_fork_namespace(script_name, root_scheme_env);

    // Evaluate script as a module
    Scheme_Object* result = scheme_eval(
        rkt_list(
            rkt_sym("register-gd-class"),
            scheme_make_utf8_string(script_source),
            rkt_quote(script_name),
            script_ns),
        root_scheme_env);
    Scheme_Object* ok = SCHEME_CAR(result);
    if (SCHEME_FALSEP(ok)) {
        rkt_eval(rkt_sym("push-exn"), SCHEME_CDR(result));
        return gdcd;
    }
    result = SCHEME_CADR(result);

    gdcd = convert_gd_class_definition(rkt_eval(
        rkt_sym("dynamic-require"),
        rkt_quote(rkt_quote(script_name)),
        rkt_quote(rkt_sym("godot-class-info"))
    ));

    script_id_class_map.insert(script.get_instance_id(), script_name);
    script_id_definition_map.insert(script.get_instance_id(), gdcd);
    return gdcd;
}


void RacketBinder::scheme_initialize_instance(SchemeScriptInstance &p_target) {
    uint64_t script_instance_id = p_target.get_script()->get_instance_id();
    if (!script_id_class_map.has(script_instance_id)) {
        rkt_eval(
            rkt_sym("push-error"),
            rkt_string("Error creating instance"));
        return;
    }
    uint32_t inst_id = p_target.get_owner()->get_instance_id();
    Scheme_Object* class_name_symbol = script_id_class_map.get(p_target.get_script()->get_instance_id());

    Scheme_Object* instance = scheme_eval(
        rkt_list(
            rkt_sym("godot-new-instance"),
            rkt_quote(class_name_symbol)
        ), root_scheme_env);

    if (SCHEME_FALSEP(SCHEME_CAR(instance))) {
        rkt_eval(
            rkt_sym("push-exn"),
            SCHEME_CDR(instance),
            rkt_string("Error creating instance"));
        return;
    } else {
        instance_id_ctx_map.insert(inst_id, SCHEME_CDR(instance));
    }
}


int32_t RacketBinder::scheme_get_member_line(SchemeScriptInstance &p_target, const StringName &member) {
    return 0;
}


Variant RacketBinder::scheme_call(SchemeScriptInstance &p_target, const String p_func_name, const Variant **p_args, int p_argcount, SchemeCallError* r_error) {
    uint32_t instance_id = p_target.get_owner()->get_instance_id();
    if (!instance_id_ctx_map.has(instance_id)) {
        _logln("No script instance found");
        return Variant();
    }
    Scheme_Object* instance = instance_id_ctx_map.get(instance_id);

    Scheme_Object* args_list = scheme_null;
    if (p_argcount > 0) {
        Scheme_Object** objs = (Scheme_Object**)malloc(sizeof(Scheme_Object*) * p_argcount);
        for (int i = 0; i < p_argcount; i++) {
            const Variant* arg_i = p_args[i];
            objs[i] = gd_obj2rkt_obj(arg_i);
        }
        args_list = scheme_build_list(p_argcount, objs);
    }

    const char* method_name_chars = gdstr2charp(p_func_name);
    Scheme_Object* method_name_sym = rkt_sym(method_name_chars);
    Scheme_Object* out = rkt_eval_handle(
        rkt_sym("gd-invoke"),
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
    if (!instance_id_ctx_map.has(p_target.get_owner()->get_instance_id())) {
        return false;
    }
    // TODO: implement
    return true;
}


bool RacketBinder::get(const SchemeScriptInstance &p_target, const StringName, Variant &r_ret) const {
    if (!instance_id_ctx_map.has(p_target.get_owner()->get_instance_id())) {
        return false;
    }
    // TODO: implement
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


void RacketBinder::scheme_free_instance(SchemeScriptInstance &p_target) {

}

