#include "binder/racket_bc_binder.h"

#include <stdio.h>
#include <condition_variable>
#include <iostream>
#include <mutex>
#include <thread>

#include "godot_cpp/classes/engine.hpp"

#include "racket_script.h"
#include "racket_script_instance.h"
#include "binder/racket_binder_util.h"
#include "binder/racket_bc_runtime.h"
#include "binder/racket_builtin_binder.h"
#include "binder/racket_gdprimitive.h"
#include "util/racket_util.h"


#define RKT_TO_HASH(sch_obj) SCHEME_HASHTRP(sch_obj) ? (Scheme_Hash_Tree*)sch_obj : nullptr;
#define RKT_HASH_GET(so, key, deflt) RacketBCRuntime::get_singleton()->evalv(rkt_sym("hash-ref"), so, rkt_quote(key), deflt)

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
#define ERR_FAIL_ERR(expr)                    \
    {                                         \
        Error st = expr;                      \
        ERR_FAIL_COND_V(st != Error::OK, st); \
    }

#define EVAL(...) RacketBCRuntime::get_singleton()->evalv(__VA_ARGS__)
#define EVAL_STRING(str) RacketBCRuntime::get_singleton()->eval_string(str)


// TODO: figure out a better way to do all of these class metadata-extracting functions, tend to be brittle
_FORCE_INLINE_ Error hash_to_gd_prop(Scheme_Object* p_obj, GDProperty& prop) {
    ERR_FAIL_COND_V_MSG(scheme_false == EVAL(rkt_sym("hash?"), p_obj), Error::FAILED, "not a hash table");
    prop.name = rktsym2gdstrname(RKT_HASH_GET(p_obj, SYM_NAME, rkt_quote_sym("error")));
    prop.type = (GDExtensionVariantType)SCHEME_INT_VAL(RKT_HASH_GET(p_obj, SYM_TYPE, rkt_int(0)));
    return Error::OK;
}


_FORCE_INLINE_ Error hash_to_gd_class_prop(Scheme_Object* p_obj, GDClassProperty& cprop) {
    ERR_FAIL_COND_V_MSG(scheme_false == (rkt_sym("hash?"), p_obj), Error::FAILED, "not a hash table");
    cprop.symbol = rktsym2gdstrname(RKT_HASH_GET(p_obj, SYM_NAME, rkt_quote_sym("error")));
    ERR_FAIL_ERR(hash_to_gd_prop(RKT_HASH_GET(p_obj, SYM_PROP, EVAL_STRING("#hash()")), cprop.property));
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
    ERR_FAIL_COND_V_MSG(scheme_false == EVAL(rkt_sym("hash?"), p_obj), Error::FAILED, "not a hash table");
    class_def.name = rktsym2gdstrname(RKT_HASH_GET(p_obj, SYM_NAME, rkt_quote_sym("error")));
    Scheme_Object* vars = RKT_HASH_GET(p_obj, SYM_VAR, RKT_EMPTY_LIST);
    Scheme_Object* export_vars = RKT_HASH_GET(p_obj, SYM_EXPORT_VAR, RKT_EMPTY_LIST);
    Scheme_Object* methods = RKT_HASH_GET(p_obj, SYM_FUNC, RKT_EMPTY_LIST);
    Scheme_Object* signals = RKT_HASH_GET(p_obj, SYM_SIGNAL, RKT_EMPTY_LIST);

    Scheme_Object* all_vars[] = {
        RKT_HASH_GET(p_obj, SYM_VAR, RKT_EMPTY_LIST),
        RKT_HASH_GET(p_obj, SYM_EXPORT_VAR, RKT_EMPTY_LIST)};

    while (vars != scheme_null) {
        GDClassProperty cprop;
        cprop.usage = (PropertyUsageFlags)(cprop.usage & ~PropertyUsageFlags::PROPERTY_USAGE_DEFAULT);
        ERR_FAIL_ERR(hash_to_gd_class_prop(SCHEME_CAR(vars), cprop));
        class_def.register_prop(cprop.property.name, cprop);
        vars = SCHEME_CDR(vars);
    }

    while (export_vars != scheme_null) {
        GDClassProperty cprop;
        ERR_FAIL_ERR(hash_to_gd_class_prop(SCHEME_CAR(export_vars), cprop));
        cprop.usage = (PropertyUsageFlags)(cprop.usage | PropertyUsageFlags::PROPERTY_USAGE_DEFAULT);
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

// Scheme_Object* generate_racket_class_wrapper(StringName& p_class_name) {
//     RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();
//     StringName parent;
//     if (p_class_name == StringName("Object")) {
//         parent = "object%";
//     } else {
//         parent = RacketClassDB::get_parent_class(p_class_name);
//     }
//     Array property_list = RacketClassDB::class_get_property_list(p_class_name, true);
//     Array method_list = RacketClassDB::class_get_method_list(p_class_name, true);

//     Scheme_Object* properties_sexpr = scheme_make_null();
//     for (int i = 0; i < property_list.size(); i++) {
//         Dictionary prop_dict = property_list[i];
//         Scheme_Object* nm_symbol = binder->variant_to_scheme_object(prop_dict.get(StringName("name"), nullptr));
//         properties_sexpr = scheme_make_list_pair(nm_symbol, properties_sexpr);
//     }
//     Scheme_Object* methods_sexpr = scheme_make_null();
//     for (int i = 0; i < method_list.size(); i++) {
//         Dictionary method_dict = property_list[i];
//         Scheme_Object* method_sexpr = scheme_make_null();
//         Array args_list = method_dict.get(StringName("args"), Array());
//         for (int j = 0; j < args_list.size(); j++) {
//             method_sexpr = scheme_make_list_pair(
//                 binder->variant_to_scheme_object(args_list[i].get(StringName("name"), nullptr)),
//                 method_sexpr);
//         }
//         method_sexpr = scheme_make_list_pair(
//             binder->variant_to_scheme_object(method_dict.get(StringName("name"), nullptr)),
//             method_sexpr);
//         methods_sexpr = scheme_make_list_pair(method_sexpr, methods_sexpr);
//     }
//     return EVAL(
//         rkt_sym("gd-api-class"),
//         binder->variant_to_scheme_object(p_class_name),
//         binder->variant_to_scheme_object(parent),
//         methods_sexpr,
//         properties_sexpr);
// }


RacketBCBinder* RacketBCBinder::singleton = nullptr;
RacketBCBinder* RacketBCBinder::get_singleton() {
    if (singleton == nullptr) {
        singleton = new RacketBCBinder();
    }
    return singleton;
}

RacketBCBinder::RacketBCBinder() {}

RacketBCBinder::~RacketBCBinder() {
    // TODO: figure out how to tear down Racket
}

GDClassDefinition* RacketBCBinder::get_definition(const RacketScript& script) {
    if (script_id_definition_map.has(script.get_instance_id())) {
        return &script_id_definition_map.get(script.get_instance_id());
    }
    return nullptr;
}

// TODO: finalize definitions when scripts are changed or deleted
Error RacketBCBinder::create_definition(const RacketScript& script, GDClassDefinition& def) {
    const char* script_source = script.get_source_code().utf8().get_data();
    const char* script_name = script.get_path().utf8().get_data();

    Scheme_Object* script_module_name = rkt_sym(script_name);
    RacketBCRuntime* runtime = RacketBCRuntime::get_singleton();

    Scheme_Object* script_ns = runtime->eval(rkt_list(rkt_sym("current-namespace")));
    Scheme_Object* result = scheme_null;
    bool ok = false;
    Scheme_Object* app_list = rkt_list(
        rkt_sym("gdracket-class-register"),
        scheme_make_utf8_string(script_source),
        rkt_quote(script_module_name),
        script_ns);

    runtime->eval_handle(app_list, &result, &ok);

    if (!ok) {
        UtilityFunctions::push_error("Could not create class definition");
        return Error::ERR_INVALID_DECLARATION;
    }

    hash_to_class_definition(result, def);

    script_id_class_map.insert(script.get_instance_id(), script_module_name);
    script_id_definition_map.insert(script.get_instance_id(), def);
    return Error::OK;
}


void RacketBCBinder::delete_definition(const RacketScript& p_script) {
    uint32_t instance_id = p_script.get_instance_id();
    script_id_class_map.erase(instance_id);
    script_id_definition_map.erase(instance_id);
}


Error RacketBCBinder::initialize_instance(RacketScriptInstance& p_target) {
    // TODO: allow non-class instances
    bool ok = false;
    uint32_t inst_id = 0;
    uint64_t script_instance_id = p_target.get_script()->get_instance_id();
    RacketBCBuiltinBinder* builtinBinder = RacketBCBuiltinBinder::get_singleton();
    RacketBCRuntime* runtime = RacketBCRuntime::get_singleton();
    Scheme_Object* instance = scheme_null,
        *class_name_symbol = scheme_null,
        *script_ns = scheme_null,
        *app_list = scheme_null,
        *cls_instance = scheme_null;

    if (!script_id_class_map.has(script_instance_id)) {
        UtilityFunctions::push_error("Error creating instance: script error");
        return Error::ERR_UNAVAILABLE;
    }

    inst_id = p_target.get_owner()->get_instance_id();
    class_name_symbol = script_id_class_map.get(p_target.get_script()->get_instance_id());
    script_ns = runtime->evalv(rkt_sym("current-namespace"));
    app_list = rkt_list(
        rkt_sym("gdracket-class-instantiate"),
        rkt_quote(class_name_symbol),
        script_ns,
        builtinBinder->variant_to_scheme_object(p_target.get_owner()));

    runtime->eval_handle(app_list, &instance, &ok);

    if (!ok) {
        UtilityFunctions::push_error(
            (String)builtinBinder->scheme_object_to_variant(
                runtime->eval(rkt_list(rkt_sym("exn-message"), instance))));
        return Error::ERR_SCRIPT_FAILED;
    } else {
        cls_instance = SCHEME_CDR(instance);
        // CRITICAL: Prevent Racket GC on instances until Godot frees the Object
        // this script is attached to
        scheme_dont_gc_ptr(cls_instance);
        instance_id_ctx_map.insert(inst_id, cls_instance);
        p_target.set_scheme_object((void*)cls_instance);
    }

    return Error::OK;
}


int32_t RacketBCBinder::get_member_line(RacketScriptInstance& p_target, const StringName& member) {
    return 0;
}


Variant RacketBCBinder::call(RacketScriptInstance& p_target, const String p_func_name, const Variant** p_args, int p_argcount, RacketCallError* r_error) {
    bool ok = false;
    uint32_t instance_id = p_target.get_owner()->get_instance_id();
    uint32_t script_id = p_target.get_script()->get_instance_id();
    RacketBCBuiltinBinder* builtinBinder = RacketBCBuiltinBinder::get_singleton();
    RacketBCRuntime* runtime = RacketBCRuntime::get_singleton();
    ERR_FAIL_COND_V_MSG(!instance_id_ctx_map.has(instance_id), Variant(), "No script instance found for instance " + p_target.get_owner()->to_string());
    Scheme_Object* instance = (Scheme_Object*)p_target.get_scheme_object();
    GDClassDefinition* def = &script_id_definition_map.get(script_id);
    Scheme_Object* out = scheme_null;
    Scheme_Object* args_list = scheme_null;

    ERR_FAIL_COND_V_MSG(!def->has_method(p_func_name), Variant(), "Instance has no method  " + p_func_name);

    if (p_argcount > 0) {
        // Create argument list by iterating through argument array in reverse
        for (int i = p_argcount - 1; i >= 0; i--) {
            const Variant* arg_i = p_args[i];
            args_list = scheme_make_pair(builtinBinder->variant_to_scheme_object(*arg_i), args_list);

        }
    }

    // Get the actual Racket name for the cleaned name registered with Godot
    const StringName method_symbol = (const StringName)def->get_method(p_func_name)->symbol;
    Scheme_Object* method_name_sym = builtinBinder->variant_to_scheme_object(method_symbol);

    Scheme_Object* app_list = rkt_list(
        rkt_sym("send/apply"),
        instance,
        method_name_sym,
        rkt_quote(args_list));
    runtime->eval_handle(app_list, &out, &ok);

    ERR_FAIL_COND_V_MSG(!ok, Variant(),
        (String)builtinBinder->scheme_object_to_variant(runtime->eval(rkt_list(rkt_sym("exn-message"), out))));

    return builtinBinder->scheme_object_to_variant(out);
}

bool RacketBCBinder::set(RacketScriptInstance& p_target, const StringName& p_name, const Variant& p_value) {
    // TODO: set up something to reduce hash table lookups?
    RacketBCBuiltinBinder* builtinBinder = RacketBCBuiltinBinder::get_singleton();
    RacketBCRuntime* runtime = RacketBCRuntime::get_singleton();
    uint32_t owner_id = p_target.get_owner()->get_instance_id();
    uint32_t script_id = p_target.get_script()->get_instance_id();

    ERR_FAIL_COND_V_MSG(!instance_id_ctx_map.has(owner_id) || !script_id_definition_map.has(script_id), false, "No script instance found for object " + p_target.get_owner()->to_string());

    GDClassDefinition* def = &script_id_definition_map.get(script_id);
    if (!def->has_prop(p_name)) {
        return false;
    }
    Scheme_Object* instance = instance_id_ctx_map.get(owner_id);
    Scheme_Object* prop_symbol = rkt_sym((gdstr2charp(((const String)def->get_prop(p_name)->symbol))));
    Scheme_Object* wrapped = builtinBinder->variant_to_scheme_object(p_value);
    Scheme_Object* app_list = rkt_list(rkt_sym("set-field!"), prop_symbol, instance, wrapped);
    Scheme_Object* res = scheme_null;
    bool ok = false;

    runtime->eval_handle(app_list, &res, &ok);
    ERR_FAIL_COND_V_MSG(!ok, false,
        (String)builtinBinder->scheme_object_to_variant(runtime->eval(rkt_list(rkt_sym("exn-message"), SCHEME_CDR(res)))));

    return true;
}

bool RacketBCBinder::get(const RacketScriptInstance& p_target, const StringName p_name, Variant* r_ret) const {
    RacketBCBuiltinBinder* builtinBinder = RacketBCBuiltinBinder::get_singleton();
    RacketBCRuntime* runtime = RacketBCRuntime::get_singleton();
    uint32_t owner_id = p_target.get_owner()->get_instance_id();
    uint32_t script_id = p_target.get_script()->get_instance_id();

    ERR_FAIL_COND_V_MSG(!instance_id_ctx_map.has(owner_id) || !script_id_definition_map.has(script_id), false, "No script instance found for object " + p_target.get_owner()->to_string());

    const GDClassDefinition* def = &script_id_definition_map.get(script_id);
    ERR_FAIL_COND_V_MSG(!def->has_prop(p_name), false, "Class " + def->name + "Does not have property " + p_name);

    Scheme_Object* instance = instance_id_ctx_map.get(owner_id);
    Scheme_Object* prop_symbol = rkt_sym((gdstr2charp(((const String)def->get_prop(p_name)->symbol))));

    Scheme_Object* app_list = rkt_list(rkt_sym("get-field"), prop_symbol, instance);
    Scheme_Object* res = scheme_null;
    bool ok = false;
    runtime->eval_handle(app_list, &res, &ok);
    ERR_FAIL_COND_V_MSG(!ok, false,
        (String)builtinBinder->scheme_object_to_variant(runtime->eval(rkt_list(rkt_sym("exn-message"), SCHEME_CDR(res)))));

    *r_ret = builtinBinder->scheme_object_to_variant(res);
    return true;
}


bool RacketBCBinder::has_method(const RacketScriptInstance& p_target, const StringName& p_method) const {
    uint32_t scr_instid = p_target.get_script()->get_instance_id();
    if (!script_id_definition_map.has(scr_instid)) {
        return false;
    }
    const GDClassDefinition* defn = &(script_id_definition_map.get(scr_instid));
    return defn->has_method(p_method);
}


void RacketBCBinder::free_instance(RacketScriptInstance& p_target) {
    uint32_t inst_id = p_target.get_owner()->get_instance_id();
    if (!instance_id_ctx_map.has(inst_id)) {
        return;
    }
    Scheme_Object* cls_instance = instance_id_ctx_map.get(inst_id);
    // CRITICAL: allow Racket to garbage-collect instance now
    scheme_gc_ptr_ok(cls_instance);
    instance_id_ctx_map.erase(inst_id);
}

// Collection Conversions //////////////////////////////////////////////////////
// These are expensive and should not be used frequently, user code should favor
// using the builtin wrappers for Array and Dictionary instead

Scheme_Object* _list_or_vector_to_list(Scheme_Object* p_obj) {
    if (SCHEME_LISTP(p_obj)) {
        return p_obj;
    }
    if (SCHEME_VECTORP(p_obj)) {
        return RacketBCRuntime::get_singleton()->eval(rkt_list(rkt_sym("vector->list"), p_obj));
    }
    return scheme_null;
}

Scheme_Object* RacketBCBinder::_extract_scheme_value_with_collections(Variant p_value) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();
    if (p_value.get_type() == Variant::Type::ARRAY) {
        return array_to_list(p_value);
    } else if (p_value.get_type() == Variant::Type::DICTIONARY) {
        return dictionary_to_hashtable(p_value);
    }
    return binder->variant_to_scheme_object(p_value);
}

Scheme_Object* RacketBCBinder::array_to_list(Array arr) {
    Scheme_Object* res = scheme_null;
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();
    for (int i = arr.size() - 1; i >= 0; i--) {
        Scheme_Object* svalue = _extract_scheme_value_with_collections(arr[i]);
        res = scheme_make_pair(svalue, res);
    }
    return res;
}

Scheme_Object* RacketBCBinder::dictionary_to_hashtable(Dictionary dict) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();
    Scheme_Hash_Table* res = scheme_make_hash_table_equal();
    Array keys = dict.keys();
    for (int i = 0; i < keys.size(); i++) {
        Variant value = dict[keys[i]];
        Scheme_Object* svalue = _extract_scheme_value_with_collections(dict[keys[i]]);
        scheme_hash_set(
            res,
            _extract_scheme_value_with_collections(keys[i]),
            _extract_scheme_value_with_collections(dict[keys[i]]));
    }
    return (Scheme_Object*)res;
}

Variant RacketBCBinder::_extract_variant_with_collections(Scheme_Object* p_obj) {
    RacketBCBuiltinBinder* b_binder = RacketBCBuiltinBinder::get_singleton();
    if (EVAL(rkt_sym("hash?"), p_obj)) {
        return hashtable_to_dictionary(p_obj);
    }
    if (EVAL(rkt_sym("list?"), p_obj)) {
        return list_or_vector_to_array(p_obj);
    }
    return b_binder->scheme_object_to_variant(p_obj);
}

Array RacketBCBinder::list_or_vector_to_array(Scheme_Object* p_obj) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();
    Array res;
    Scheme_Object* lst = _list_or_vector_to_list(p_obj);
    ERR_FAIL_COND_V_MSG(SCHEME_NULLP(lst), res, "not a list or vector");
    for (Scheme_Object* value = SCHEME_CAR(lst); SCHEME_NULLP(value); value = SCHEME_CADR(lst)) {
        Variant vvalue = _extract_variant_with_collections(value);
        res.append(vvalue);
    }
    return res;
}

Dictionary RacketBCBinder::hashtable_to_dictionary(Scheme_Object* p_obj) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();
    Dictionary res;
    ERR_FAIL_COND_V_MSG(scheme_false == EVAL(rkt_sym("hash?"), p_obj), res, "not a hash table");
    Scheme_Object* keys = EVAL(rkt_sym("hash-keys"), p_obj);

    Scheme_Object* key = SCHEME_CAR(keys);
    for (Scheme_Object* key = SCHEME_CAR(keys); SCHEME_NULLP(key); key = SCHEME_CADR(keys)) {
        Variant vkey = _extract_variant_with_collections(key);
        Scheme_Object* value = EVAL(rkt_sym("hash-ref"), p_obj, key);
        Variant vvalue = _extract_variant_with_collections(value);
        res[vkey] = vvalue;
    }
    return res;
}
