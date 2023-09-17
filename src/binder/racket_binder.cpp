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


Scheme_Env* init_gdprimitive_module(Scheme_Env* env) {
    Scheme_Env* modul = scheme_primitive_module(rkt_sym("gd-primitive"), env);
    scheme_add_global(
        "push-error",
        scheme_make_prim_w_everything(_gdprimitive_push_error, 1, "push-error", 1, 1, 0, 0, 0),
        modul);
    scheme_add_global(
        "get-singleton",
        scheme_make_prim_w_everything(_gdprimitive_push_error, 1, "get-singleton", 1, 1, 0, 1, 1),
        modul);
    scheme_finish_primitive_module(modul);
    return modul;
}


// Environment setup ///////////////////////////////////////////////////////////

void _env_setup_globals(Scheme_Env* env, Scheme_Object* collects_path_string_list) {

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

    scheme_add_global("gd-type-variant", scheme_make_integer(Variant::VARIANT_MAX), env);
    scheme_add_global("gd-type-float", scheme_make_integer(Variant::FLOAT), env);


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

    _logln("namespace created");
    return dest_ns;
}


RacketEnvironment::RacketEnvironment() {
    sem = new(std::condition_variable);
    mutex = new(std::mutex);
    ready = false;
    setup_done = false;

    // worker = new std::thread([this]() {
        _setup_tls_space();

        scheme_main_stack_setup(1, [](void* data) -> int {

            RacketEnvironment* setup_data = (RacketEnvironment*)data;

            Scheme_Object* collects_paths = rkt_list(
                gdstr2rktstr(SchemeLanguage::get_singleton()->get_install_dir()));

            // Wait for calling thread to catch up
            // std::unique_lock<std::mutex> lk(*(setup_data->mutex));
            // setup_data->sem->wait(lk, [setup_data]{ return setup_data->ready; });

            root_scheme_env = scheme_basic_env();
            // // TODO: Create port that pipes racket output to godot's
            root_env_output = scheme_make_file_output_port(stdout);
            scheme_set_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT, root_env_output);
            _env_setup_globals(root_scheme_env, collects_paths);
            // declare_modules(root_scheme_env);
            Scheme_Env* env = root_scheme_env;
            setup_data->set_env(env);

            // setup_data->setup_done = true;
            // lk.unlock();
            // setup_data->sem->notify_one();

            // Scheme_Object* curout = scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT);


            // while (true) {
            //     {
            //         // std::unique_lock<std::mutex> lk(*(setup_data->mutex));
            //         // setup_data->sem->wait(lk, [setup_data]{
            //         //     return setup_data->request.state == RequestState::PENDING;
            //         // });

            //         Scheme_Object* res;
            //         if (setup_data->request.request_type == RequestType::STRING) {
            //             const char* eval_str = setup_data->request.string_request.c_str();
            //             Scheme_Object* sexpr_string = scheme_make_utf8_string(eval_str);
            //             scheme_eval(sexpr_list(rkt_sym("display"), sexpr_string), setup_data->request.ns);
            //             // TODO: Exception handling on read
            //             res = scheme_eval(sexpr_list(rkt_sym("eval-source"), sexpr_string), setup_data->request.ns);
            //             scheme_eval(sexpr_list(rkt_sym("display"), sexpr_list(rkt_sym("quote"), res)), setup_data->request.ns);
            //         } else if (setup_data->request.request_type == RequestType::SEXPR) {
            //             res = scheme_eval_multi(setup_data->request.sexpr_request, setup_data->request.ns);
            //         } else if (setup_data->request.request_type == RequestType::FUNCTION) {
            //             res = setup_data->request.func_request(setup_data->request.sexpr_request, setup_data->request.ns);
            //         }

            //         scheme_flush_output(curout);
            //         setup_data->request.result = res;
            //         setup_data->request.state = RequestState::COMPLETE;
            //         // lk.unlock();
            //         // setup_data->sem->notify_one();
            //     }

            // }
            return 0;
        }, (void*)this);
    // });

    // Allow thread to initialize Racket and wait for it to enter busy-wait loop
    // {
    //     std::lock_guard<std::mutex> lk(*mutex);
    //     // _logln("Thread notified setting ready");
    //     ready = true;
    // }
    // sem->notify_one();
    // // _logln("Thread notified, waiting");
    // {
    //     std::unique_lock<std::mutex> lk(*mutex);
    //     sem->wait(lk, [this]{ return setup_done; });
    // }

}


Scheme_Object* RacketEnvironment::eval_in(Scheme_Object* eval_list, Scheme_Env* ns) {
    request.result = scheme_eval(eval_list, ns);
    // {
    //     std::lock_guard<std::mutex> lk(*mutex);
    //     request.state = RequestState::PENDING;
    //     request.ns = ns;
    //     request.request_type = RequestType::SEXPR;
    //     request.sexpr_request = eval_list;
    // }
    // // _logln("Sending request to thread");
    // sem->notify_one();
    // {
    //     std::unique_lock<std::mutex> lk(*mutex);
    //     sem->wait(lk, [this]{
    //         return request.state == RequestState::COMPLETE;
    //     });
    // }
    return request.result;
}


Scheme_Object* RacketEnvironment::eval(Scheme_Object* eval_list) {
    return eval_in(eval_list, env);
}


Scheme_Object* RacketEnvironment::eval_string_in(std::string eval_str, Scheme_Env* ns) {
    request.result = scheme_eval_string(eval_str.c_str(), ns);
    // {
    //     std::lock_guard<std::mutex> lk(*mutex);
    //     request.state = RequestState::PENDING;
    //     request.request_type = RequestType::STRING;
    //     request.ns = ns;
    //     request.string_request = eval_str;
    // }
    // sem->notify_one();
    // {
    //     std::unique_lock<std::mutex> lk(*mutex);
    //     sem->wait(lk, [this]{
    //         return request.state == RequestState::COMPLETE;
    //     });
    // }
    return request.result;
}


Scheme_Object* RacketEnvironment::eval_string(std::string eval_str) {
    return eval_string_in(eval_str, env);
}


Scheme_Object* RacketEnvironment::eval_func_in(RacketMapper* mapper_func, Scheme_Object* args, Scheme_Env* ns) {
    // {
    //     std::lock_guard<std::mutex> lk(*mutex);
    //     request.state = RequestState::PENDING;
    //     request.request_type = RequestType::FUNCTION;
    //     request.ns = ns;
    //     request.func_request = mapper_func;
    //     request.sexpr_request = args;
    // }
    // sem->notify_one();
    // {
    //     std::unique_lock<std::mutex> lk(*mutex);
    //     sem->wait(lk, [this]{
    //         return request.state == RequestState::COMPLETE;
    //     });
    // }
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
        // _logln("exception creating class definition");
        rkt_eval(rkt_sym("displayln"), SCHEME_CDR(result));
        rkt_eval(rkt_sym("flush-output"));
        Scheme_Object* exn_message = rkt_eval(
            rkt_sym("format"),
            scheme_make_utf8_string("Exception thrown during class definition: ~a"),
            rkt_list(
                rkt_sym("exn-message"),
                SCHEME_CDR(result)));
        UtilityFunctions::push_error(rktstr2gdstr(exn_message));
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


// void _push_racket_error(Scheme_Object* exn) {
//     if (SCHEME_TRUEP(rkt_eval(rkt_sym("exn?"), exn))) {
//         // TODO: get stack trace
//         Scheme_Object* err_string = rkt_eval(
//             rkt_sym("format"),
//             scheme_make_utf8_string("")
//             rkt_sym("exn-message"),
//             exn
//         );
//         UtilityFunctions::push_error();
//     }
// }


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
            // float flval = (float)(*arg_i);
            // printf("%f\n", flval);
            // switch (arg_i->get_type()) {
            //     case Variant::Type::NIL: printf("type: NIL\n"); break;
            //     case Variant::Type::BOOL: printf("type: BOOL\n"); break;
            //     case Variant::Type::INT: printf("type: INT\n"); break;
            //     case Variant::Type::FLOAT: printf("type: FLOAT\n"); break;
            //     case Variant::Type::STRING: printf("type: STRING\n"); break;
            //     case Variant::Type::VECTOR2: printf("type: VECTOR2\n"); break;
            //     case Variant::Type::VECTOR2I: printf("type: VECTOR2I\n"); break;
            //     case Variant::Type::RECT2: printf("type: RECT2\n"); break;
            //     case Variant::Type::RECT2I: printf("type: RECT2I\n"); break;
            //     case Variant::Type::VECTOR3: printf("type: VECTOR3\n"); break;
            //     case Variant::Type::VECTOR3I: printf("type: VECTOR3I\n"); break;
            //     case Variant::Type::TRANSFORM2D: printf("type: TRANSFORM2D\n"); break;
            //     case Variant::Type::VECTOR4: printf("type: VECTOR4\n"); break;
            //     case Variant::Type::VECTOR4I: printf("type: VECTOR4I\n"); break;
            //     case Variant::Type::PLANE: printf("type: PLANE\n"); break;
            //     case Variant::Type::QUATERNION: printf("type: QUATERNION\n"); break;
            //     case Variant::Type::AABB: printf("type: AABB\n"); break;
            //     case Variant::Type::BASIS: printf("type: BASIS\n"); break;
            //     case Variant::Type::TRANSFORM3D: printf("type: TRANSFORM3D\n"); break;
            //     case Variant::Type::PROJECTION: printf("type: PROJECTION\n"); break;
            //     case Variant::Type::COLOR: printf("type: COLOR\n"); break;
            //     case Variant::Type::STRING_NAME: printf("type: STRING_NAME\n"); break;
            //     case Variant::Type::NODE_PATH: printf("type: NODE_PATH\n"); break;
            //     case Variant::Type::RID: printf("type: RID\n"); break;
            //     case Variant::Type::OBJECT: printf("type: OBJECT\n"); break;
            //     case Variant::Type::CALLABLE: printf("type: CALLABLE\n"); break;
            //     case Variant::Type::SIGNAL: printf("type: SIGNAL\n"); break;
            //     case Variant::Type::DICTIONARY: printf("type: DICTIONARY\n"); break;
            //     case Variant::Type::ARRAY: printf("type: ARRAY\n"); break;
            //     case Variant::Type::PACKED_BYTE_ARRAY: printf("type: PACKED_BYTE_ARRAY\n"); break;
            //     case Variant::Type::PACKED_INT32_ARRAY: printf("type: PACKED_INT32_ARRAY\n"); break;
            //     case Variant::Type::PACKED_INT64_ARRAY: printf("type: PACKED_INT64_ARRAY\n"); break;
            //     case Variant::Type::PACKED_FLOAT32_ARRAY: printf("type: PACKED_FLOAT32_ARRAY\n"); break;
            //     case Variant::Type::PACKED_FLOAT64_ARRAY: printf("type: PACKED_FLOAT64_ARRAY\n"); break;
            //     case Variant::Type::PACKED_STRING_ARRAY: printf("type: PACKED_STRING_ARRAY\n"); break;
            //     case Variant::Type::PACKED_VECTOR2_ARRAY: printf("type: PACKED_VECTOR2_ARRAY\n"); break;
            //     case Variant::Type::PACKED_VECTOR3_ARRAY: printf("type: PACKED_VECTOR3_ARRAY\n"); break;
            //     case Variant::Type::PACKED_COLOR_ARRAY: printf("type: PACKED_COLOR_ARRAY\n"); break;
            //     case Variant::Type::VARIANT_MAX: printf("type: VARIANT_MAX\n"); break;
            // }
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

    // rkt_eval(rkt_sym("displayln"), rkt_quote(out));
    // rkt_eval(rkt_sym("flush-output"));

    if (SCHEME_FALSEP(SCHEME_CAR(out))) {
        Scheme_Object* err_msg = rkt_eval(rkt_sym("exn->string"), SCHEME_CDR(out));
        UtilityFunctions::push_error(rktstr2gdstr(err_msg));
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

