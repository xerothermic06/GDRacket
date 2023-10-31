#include "binder/racket_bc_runtime.h"

#include <stdio.h>

#include "godot_cpp/classes/engine.hpp"

#include "racket_script.h"
#include "racket_script_instance.h"
#include "binder/racket_binder_util.h"
#include "binder/racket_builtin_binder.h"
#include "binder/racket_gdprimitive.h"
#include "util/racket_util.h"

// Include Racket bytecode-bootstrapping sources
// racoBC ctool --c-mods racket_modules.c ++lib racket/main ++lib racket/base ++lib racket/class ++lib racket/vector ++lib racket/lang/reader ++lib racket/runtime-config
#include "racket_modules.c"
// TODO: Should really just have a .zo file with all the builtin deps we need for flexibility
// see if it's possible to compile base deps into a single file

void _setup_tls_space() {
#ifdef _WIN32
    // TODO: non-windows version of this if needed
    static __declspec(thread) void* tls_space;
    scheme_register_tls_space(tls_space, 0);
#endif
}

// TODO: figure out if it's at all possible to instantiate Scheme_Env in heap
// (unlikely since BC racket seems to put all its main stuff in statics :cry:)
// this will need significant rework to make multi-threaded
Scheme_Env* RacketBCRuntime::root_scheme_env;
Scheme_Object* RacketBCRuntime::root_env_output;

void _env_setup_globals(Scheme_Env* env, Scheme_Object* collects_path_string_list) {
    gdprimitive_init_module(env);

    declare_modules(env);

    scheme_namespace_require(scheme_intern_symbol("racket/base"));
    scheme_namespace_require(scheme_intern_symbol("racket/exn"));
    scheme_namespace_require(scheme_intern_symbol("racket/class"));
    scheme_namespace_require(scheme_intern_symbol("racket/vector"));

    // // TODO: create bytecode embed for system modules
    scheme_init_collection_paths(env, collects_path_string_list);
    scheme_namespace_require(rkt_sym("gdracket/gd-native-interface"));
    scheme_namespace_require(rkt_sym("gdracket/gd-script-classes"));
    scheme_namespace_require(rkt_sym("gdracket/gd-util"));
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
                            rkt_sym("gdracket/godot")))),
                parent_env);

    return dest_ns;
}

RacketBCRuntime *RacketBCRuntime::singleton = nullptr;
RacketBCRuntime *RacketBCRuntime::get_singleton() {
    if (singleton == nullptr) {
        singleton = new RacketBCRuntime();
    }
    return singleton;
}

RacketBCRuntime::RacketBCRuntime() {
    _setup_tls_space();
    scheme_main_stack_setup(
            1,
            [](void *data) -> int {
                RacketBCRuntime *setup_data = (RacketBCRuntime *)data;

                Scheme_Object *collects_paths =
                        rkt_list(gdstr2rktstr(RacketLanguage::get_singleton()->get_install_dir()));

                root_scheme_env = scheme_basic_env();
                // // TODO: Create port that pipes racket output to godot's
                root_env_output = scheme_make_file_output_port(stdout);

                scheme_set_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT, root_env_output);
                // scheme_set_compiled_file_check(SCHEME_COMPILED_FILE_CHECK_EXISTS);
                scheme_set_compiled_file_check(SCHEME_COMPILED_FILE_CHECK_MODIFY_SECONDS);

                RacketBCBuiltinBinder::get_singleton()->register_builtin_types();
                _env_setup_globals(root_scheme_env, collects_paths);

                return 0;
            },
            (void *)this);
}

RacketBCRuntime::~RacketBCRuntime() {
    // TODO: figure out how to tear down Racket
}


Scheme_Object *RacketBCRuntime::eval(Scheme_Object *p_app_list) {
    return scheme_eval(p_app_list, root_scheme_env);
}


Scheme_Object* RacketBCRuntime::eval_string(char* str) {
    return scheme_eval_string(str, root_scheme_env);
}


void RacketBCRuntime::eval_handle(Scheme_Object *p_app_list, Scheme_Object **result_or_exn, bool *ok) {
    Scheme_Object* res = scheme_eval(rkt_list(rkt_sym("eval-handle"), rkt_quote(p_app_list)), root_scheme_env);
    *ok = SCHEME_TRUEP(SCHEME_CAR(res));
    *result_or_exn = SCHEME_CDR(res);
}
