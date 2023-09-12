// This should be the only source file that touches any part of RacketBC

#include "racket_binder.h"


#include "scheme_script.h"
#include "scheme_script_instance.h"
#include "util/scheme_util.h"

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
// Make a Racket UTF-8 string from a Godot string
#define gdstr2rktstr(x) scheme_make_utf8_string(x.utf8().get_data())


static const char* VECTOR_MAP_BREAKABLE = "vector-map-breakable";
static const char* READ_MULTI = "read-multi";
static const char* EVAL_HANDLE = "eval-handle";
static const char* EVAL_SOURCE = "eval-source";

static const char* RACKET_PROC_VECTOR_MAP_BREAKABLE =
"(lambda (vec proc break-proc) "
"  (letrec "
"     ([iter "
"       (lambda (result vec) "
"         (if (and "
"              (not (void? result)) "
"              (or (eq? (vector-length vec) 0) (break-proc result))) "
"             result "
"             (iter (proc (vector-ref vec 0)) (vector-drop vec 1))))]) "
"    (iter (void) vec)))";

// Racket procedure to read a string with multiple s-expressions and move them to a vector.
static const char* RACKET_PROC_READ_MULTI =
"(lambda (in) "
"  (letrec ([read-rest (lambda (in vec) "
"                        (let ([valu (read in)]) "
"                          (if (eq? valu eof) "
"                              vec "
"                              (read-rest in (vector-append vec `#(,valu) )))))]) "
"    (read-rest in #() )))";

// Racket procedure to eval an s-expression and return a vector; If an exception is thrown,
// the vector is #(#f <exn>) else #(#t <result>)
static const char* RACKET_PROC_EVAL_HANDLE =
"(lambda (sexpr name-space) "
"    (with-handlers ([void (lambda (exn) (cons #f exn))]) "
"        (cons #t (eval sexpr name-space))))";

// Racket procedure to evaluate all the s-expressions from a source string and break with
// an exception
static const char* RACKET_PROC_EVAL_SOURCE =
"(lambda (source-str name-space) "
"    (let ([exprs-vec         (read-multi (open-input-string source-str))] "
"          [break-cond        (lambda (result-vec) (not (car result-vec)))] "
"          [eval-handle-in-ns (lambda (sexpr) (eval-handle sexpr name-space))]) "
"      (displayln exprs-vec) "
"      (vector-map-breakable exprs-vec eval-handle-in-ns break-cond)))";

// Racket procedure that attaches a list of modules from (current-namespace) to the given namespace
static const char* RACKET_PROC_NS_ATTACH_MODULES =
"(lambda (src-ns dest-ns module-list) "
"  (map (lambda (modul) (displayln modul) (namespace-attach-module src-ns modul dest-ns)) "
"  module-list))";

static const char* RACKET_GDVARIANT_TYPE_NAME = "gd-variant";



static const GDClassDefinition racket_proc_create_gdmethod(int argc, Scheme_Object **argv) {

}

static const GDClassDefinition racket_proc_create_gdclass_definition(int argc, Scheme_Object **argv) {

}


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


static Scheme_Env* root_scheme_env;
static Scheme_Object* root_env_output;


void _env_setup_globals(Scheme_Env* env, Scheme_Object* collects_path_string_list) {
    declare_modules(env);

    scheme_namespace_require(scheme_intern_symbol("racket/base"));
    // scheme_namespace_require(scheme_intern_symbol("racket/lang"));
    scheme_namespace_require(scheme_intern_symbol("racket/class"));
    scheme_namespace_require(scheme_intern_symbol("racket/vector"));

    // scheme_eval(rkt_list(rkt_sym("require"), rkt_sym("racket/base")), env);
    // scheme_eval(rkt_list(rkt_sym("require"), rkt_sym("racket/lang")), env);
    // scheme_eval(rkt_list(rkt_sym("require"), rkt_sym("racket/class")), env);
    // scheme_eval(rkt_list(rkt_sym("require"), rkt_sym("racket/vector")), env);

    // scheme_eval(rkt_list(rkt_sym("displayln"), rkt_list(rkt_sym("find-library-collection-paths"))), env);
    // scheme_flush_output(root_env_output);
    // // TODO: create bytecode embed for system modules
    // scheme_init_collection_paths(env, collects_path_string_list);

    scheme_init_collection_paths(env, collects_path_string_list);

    // scheme_namespace_require(scheme_make_utf8_string("scheme/gdracket/mod.rkt"));
    // scheme_namespace_require(scheme_make_utf8_string("scheme/gdracket/godot.rkt"));
    scheme_namespace_require(rkt_sym("gdracket/mod"));
    scheme_namespace_require(rkt_sym("gdracket/godot"));
    // scheme_eval(rkt_list(rkt_sym("require"), rkt_sym("gdracket/mod")), env);
    // scheme_eval(rkt_list(rkt_sym("require"), rkt_sym("gdracket/godot")), env);


    // scheme_eval(rkt_list(rkt_sym("require"), rkt_sym("setup/dirs")), env);

    // Scheme_Object* gdracket_path = scheme_eval(
    //     rkt_list(
    //         rkt_sym("build-path"),
    //         rkt_list(
    //             rkt_sym("find-dll-dir")
    //         ),
    //         scheme_make_utf8_string("gdracket/mod.rkt")), env);
    // scheme_eval(rkt_list(rkt_sym("require"), gdracket_path), env);

    // Scheme_Object* proc_eval_handle = scheme_eval_string(RACKET_PROC_EVAL_HANDLE, env);
    // Scheme_Object* proc_read_multi = scheme_eval_string(RACKET_PROC_READ_MULTI, env);
    // Scheme_Object* proc_vector_map_breakable = scheme_eval_string(RACKET_PROC_VECTOR_MAP_BREAKABLE, env);
    // Scheme_Object* proc_eval_source = scheme_eval_string(RACKET_PROC_EVAL_SOURCE, env);
    // scheme_add_global(EVAL_HANDLE, proc_eval_handle, env);
    // scheme_add_global(READ_MULTI, proc_read_multi, env);
    // scheme_add_global(VECTOR_MAP_BREAKABLE, proc_vector_map_breakable, env);
    // scheme_add_global(EVAL_SOURCE, proc_eval_source, env);

    // scheme_eval(rkt_list(
    //     rkt_sym("parameterize"),
    //     rkt_list(
    //         rkt_list(
    //             rkt_sym("gd-type-variant"), scheme_make_integer(Variant::VARIANT_MAX),
    //             rkt_sym("gd-type-float"), scheme_make_integer(Variant::FLOAT)
    //         )
    //     )
    // ));

    scheme_add_global("gd-type-variant", scheme_make_integer(Variant::VARIANT_MAX), env);
    scheme_add_global("gd-type-float", scheme_make_integer(Variant::FLOAT), env);

    // Scheme_Object* struct_gd_proc_info = scheme_eval_string(
    //     "(struct gd-prop-info (name type) #:reflection-name '<gd-prop-info>)"
    //     , env);
    // Scheme_Object* struct_gd_method_info = scheme_eval_string(
    //     "(struct gd-method-info (name return-type arguments) #:reflection-name '<gd-method-info>)"
    //     , env);
    // Scheme_Object* struct_gd_class_info = scheme_eval_string(
    //     "(struct gd-class-info (name methods properties) #:reflection-name '<gd-class-info>)"
    //     , env);

    // Scheme_Object* class_info = scheme_lookup_global(rkt_sym("class-info"), env);
}

Scheme_Object* _env_fork_namespace(const char* ns_name, Scheme_Env* parent_env) {
    // Scheme_Object* attach_proc = scheme_eval_string(RACKET_PROC_NS_ATTACH_MODULES, parent_env);
    // Scheme_Object* script_ns_name = scheme_make_utf8_string("some-script"); //scheme_make_utf8_string(script.get_name().utf8().get_data());
    Scheme_Object* src_ns = scheme_eval_string("(current-namespace)", parent_env);
    Scheme_Object* dest_ns = scheme_eval_string("(make-base-namespace)", parent_env);

    // Scheme_Env* modul = scheme_primitive_module(rkt_sym("gd"), parent_env);
    // Scheme_Object* struct_gd_proc_info = scheme_eval_string("(struct gd-prop-info (name type) #:reflection-name '<gd-prop-info>)", modul);
    // Scheme_Object* struct_gd_method_info = scheme_eval_string("(struct gd-method-info (name return-type arguments) #:reflection-name '<gd-method-info>)", modul);
    // Scheme_Object* struct_gd_class_info = scheme_eval_string("(struct gd-class-info (name methods properties) #:reflection-name '<gd-class-info>)", modul);
    // scheme_finish_primitive_module(modul);

    scheme_eval(rkt_list(
        // attach_proc,
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


    // scheme_eval(rkt_list(rkt_sym("eval"), struct_gd_proc_info, dest_ns), parent_env);
    // scheme_eval(rkt_list(rkt_sym("eval"), struct_gd_method_info, dest_ns), parent_env);
    // scheme_eval(rkt_list(rkt_sym("eval"), struct_gd_class_info, dest_ns), parent_env);

    _logln("namespace created");
    return dest_ns;
}


RacketRequest::RacketRequest() {}


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

    script_name = rkt_quote(rkt_sym("test-script"));
    Scheme_Object* script_ns = scheme_eval(rkt_list(rkt_sym("current-namespace")), root_scheme_env);
    // Scheme_Object* script_ns = _env_fork_namespace(script_name, root_scheme_env);

    // Evaluate script as a module
    Scheme_Object* result = scheme_eval(
        rkt_list(
            rkt_sym(EVAL_SOURCE),
            scheme_make_utf8_string(script_source),
            script_name,
            script_ns),
        root_scheme_env);
    Scheme_Object* ok = SCHEME_CAR(result);
    if (SCHEME_FALSEP(ok)) {
        _logln("exception creating class definition");
        Scheme_Object* lst = rkt_list(rkt_sym("displayln"), rkt_quote(result));
        environment->eval(lst);
        return gdcd;
    }

    // Try to require module
    result = scheme_eval(rkt_list(
            // rkt_sym(EVAL_SOURCE),
            // rkt_list(
                rkt_sym("require"),
                script_name
            // ),
            // script_ns
        ), root_scheme_env
    );
    ok = SCHEME_CAR(result);
    if (SCHEME_FALSEP(ok)) {
        _logln("exception requiring class module");
        Scheme_Object* lst = rkt_list(rkt_sym("displayln"), rkt_quote(result));
        environment->eval(lst);
        return gdcd;
    }

    // Try to get class info from module
    result = scheme_eval(rkt_list(
        rkt_sym(EVAL_HANDLE),
        rkt_sym("gd-class-info"),
        script_ns
    ), root_scheme_env);
    ok = SCHEME_CAR(result);
    if (SCHEME_FALSEP(ok)) {
        _logln("exception requiring class module");
        Scheme_Object* lst = rkt_list(rkt_sym("displayln"), rkt_quote(result));
        environment->eval(lst);
        return gdcd;
    }

    // Convert class info
    result = SCHEME_CADR(result);
    Scheme_Object* output = scheme_make_file_output_port(stdout);
    scheme_eval(rkt_list(rkt_sym("displayln"), result, output), root_scheme_env);
    scheme_flush_output(root_env_output);

    _logln("result got");
    // scheme_finish_primitive_module(script_ns);
    // scheme_eval(rkt_list(rkt_sym("define"), rkt_sym("foo"), scheme_make_integer(1)), script_ns);
    // scheme_add_global("foo", scheme_make_integer(1), script_ns);
    // _logln("defined foo");
    // Scheme_Object* foo_maybe = scheme_lookup_global(rkt_sym("foo"), script_ns);
    // _logln("got foo_maybe");

    // scheme_eval(rkt_list(rkt_sym("class-info")), script_ns);
    // if (foo_maybe == NULL) {
    //     _logln("foo null");
    // } else if (SCHEME_INTP(foo_maybe)) {
    //     int v;
    //     char buff[256];
    //     scheme_get_int_val(foo_maybe, (intptr_t*)(&v));
    //     std::snprintf(buff, 256, "foo value: %d", v);
    //     _logln(buff);
    // }
    // scheme_flush_output(root_env_output);

    // Scheme_Object* class_info = scheme_eval(rkt_sym("class-info")), script_ns); //scheme_lookup_global(rkt_sym("class-info"), script_ns);
    // _logln("got class-info");
    // if (class_info == NULL) {
    //     _logln("No class-info global found");
    //     return gdcd;
    // }

    _logln("get class-info-name");
    // Scheme_Object* nm = scheme_eval(rkt_list(rkt_sym(EVAL_HANDLE), rkt_list(rkt_sym("gd-class-name")), script_ns), root_scheme_env);
    Scheme_Object* nm = scheme_eval(
        rkt_list(
            rkt_sym(EVAL_HANDLE),
            rkt_list(
                rkt_sym("gd-class-name"),
                rkt_sym("gd-class-info")
            ),
            script_ns
         ), root_scheme_env); // (rkt_list(rkt_sym(EVAL_HANDLE), rkt_list(rkt_sym("gd-class-name")), script_ns), root_scheme_env);
    scheme_eval(rkt_list(rkt_sym("displayln"), nm, output), root_scheme_env);
    scheme_flush_output(root_env_output);

    _logln("got class-info-name");

    if (SCHEME_CHAR_STRINGP(nm)) {
        unsigned char str_buf[256];
        int chars = scheme_utf8_encode_all(SCHEME_CHAR_STR_VAL(nm), 256, str_buf);

        _logln(std::string(reinterpret_cast<char*>(str_buf)));
    }

    // if (class_info == NULL) {
    //     _logln("No class-info global found");
    //     char* str_val = SCHEME_BYTE_STR_VAL(environment->eval_sexpr_v(
    //         scheme_intern_symbol("format"),
    //         scheme_make_utf8_string("~v"),
    //         result
    //     ));
    //     _logln(std::string(str_val));
    //     return gdcd;

    // }
    // script_id_namespace_map.insert(script.get_instance_id(), script_ns);
    // script_id_class_map.insert(script.get_instance_id(), result);
    return gdcd;
}


void RacketBinder::scheme_initialize_instance(SchemeScriptInstance &p_target) {
    uint64_t script_instance_id = p_target.get_script()->get_instance_id();
    if (!script_id_class_map.has(script_instance_id)) {
        _logln("No script instance found");
        return;
    }
    uint32_t inst_id = p_target.get_owner()->get_instance_id();
    Scheme_Object* class_object = script_id_class_map.get(p_target.get_script()->get_instance_id());
    // TODO handle if class_object null
    Scheme_Object* make_instance_sexpr = scheme_make_pair(
        scheme_intern_symbol("new"),
        class_object
    );
    // Scheme_Object* object = scheme_eval(make_instance_sexpr, environment->get_env());
    Scheme_Object* object = environment->eval_sexpr_v(scheme_intern_symbol("new"), class_object);
    Scheme_Object* is_object = environment->eval_sexpr_v(scheme_intern_symbol("object?"), object);

    if (SCHEME_FALSEP(is_object)) {
        // TODO: push error
    } else {
        instance_id_ctx_map.insert(inst_id, object);
    }
}


int32_t RacketBinder::scheme_get_member_line(SchemeScriptInstance &p_target, const StringName &member) {
    return 0;
}


Variant RacketBinder::scheme_call(SchemeScriptInstance &p_target, const String p_func_name, const Variant **p_args, int p_argcount, SchemeCallError &r_error) {
    return Variant();
}


void RacketBinder::scheme_free_instance(SchemeScriptInstance &p_target) {

}

