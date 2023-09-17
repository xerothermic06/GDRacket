#include "./chibi_scheme_binder.h"

// #include <godot_cpp/core/class_db.hpp>
// #include <godot_cpp/core/defs.hpp>
// #include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/engine.hpp>
#include <godot_cpp/classes/os.hpp>
#include <godot_cpp/classes/project_settings.hpp>
// #include <godot_cpp/classes/resource_loader.hpp>
// #include <godot_cpp/classes/resource_saver.hpp>

#include <iostream>

using namespace godot;

String _get_default_lib_path() {
    String res;
    if (OS::get_singleton()->has_feature("editor")){
        res = ProjectSettings::get_singleton()->globalize_path("res://scheme/");
    } else {
        res = OS::get_singleton()->get_executable_path().get_base_dir().path_join("scheme");
    }
    UtilityFunctions::print_verbose(String("Found path for scheme libs: ") + String(res));
    return res;
}


const char* _string_to_charptr(const String s) {
    CharString st = s.utf8();
    return st.get_data();
}

// This will not do any sexp allocations, assign to an existing sexp created with sexp_gc_varn
sexp _string_to_sexp(sexp ctx, const String s) {
    const char* ptr = _string_to_charptr(s);
    return sexp_c_string(ctx, ptr, s.length());
}


String _sexp_to_string(sexp s) {
    if (!sexp_stringp(s)) {
        return String();
    }
    return sexp_string_data(s);
}


void ChibiSchemeBinder::initialize() {
    sexp_gc_var1(path_str);

    sexp_scheme_init();
    ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);

    // TODO: find better way to set up default library path
    String install_path = _get_default_lib_path();
    path_str = _string_to_sexp(ctx, install_path);

    UtilityFunctions::print_verbose("adding module path " + install_path); // DEBUG

    sexp_add_module_directory(ctx, path_str, SEXP_TRUE);
    env = sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);

    if (sexp_exceptionp(env)) {
        UtilityFunctions::push_error(String("Error initializing scheme binder: ") + _sexp_to_string(sexp_exception_message(env)));
    }
}


void ChibiSchemeBinder::uninitialize() {
    sexp_destroy_context(ctx);
}


void ChibiSchemeBinder::scheme_initialize_instance(SchemeScriptInstance &p_target) {
    sexp_gc_var1(res);
    sexp_gc_preserve1(ctx, res);

    uint32_t inst_id = p_target.get_owner()->get_instance_id();
    const char* script_source = _string_to_charptr(p_target.get_script()->get_source_code());
    // script_source = "(define test (lambda () 1))";

    sexp inst_ctx = sexp_make_eval_context(
        ctx,
        sexp_context_stack(ctx),
        sexp_context_env(env),
        0,
        sexp_context_max_size(ctx));
    res = sexp_eval_string(inst_ctx, script_source, -1, env);

    if (sexp_exceptionp(res)) {
        String msg = _sexp_to_string(sexp_exception_message(res));
        UtilityFunctions::push_error("Error parsing scheme script: " + msg);
        sexp_gc_release1(ctx);
        return;
    }

    instance_id_ctx_map.insert(inst_id, inst_ctx);

    sexp_gc_release1(ctx);
}


int32_t ChibiSchemeBinder::scheme_get_member_line(SchemeScriptInstance &p_target, const StringName &member) {
    return 0;
}


GDClassDefinition ChibiSchemeBinder::scheme_create_definition(const SchemeScript &script) {
    GDClassDefinition definition;
    definition.name = "SchemeClass";

    GDMethod test_method;
    test_method.name = "test";

    GDProperty return_val;
    return_val.type = GDExtensionVariantType::GDEXTENSION_VARIANT_TYPE_INT;
    test_method.return_val = return_val;

    definition.methods.insert("test", test_method);

    return definition;
}


Variant ChibiSchemeBinder::scheme_call(SchemeScriptInstance &p_target, const String p_func_name,  const Variant *p_args, int p_argcount, SchemeCallError &r_error) {
    sexp_gc_var2(proc, invoke);
    sexp_gc_preserve2(ctx, proc, invoke);
    Variant result;

    if (!instance_id_ctx_map.has(p_target.get_owner()->get_instance_id())) {
        goto scheme_call_end;
    }

    sexp inst_ctx = instance_id_ctx_map.get(p_target.get_owner()->get_instance_id());
    proc = sexp_intern(inst_ctx, _string_to_charptr(p_func_name), -1);
    invoke = sexp_list1(inst_ctx, proc);
    sexp res = sexp_eval(inst_ctx, invoke, env);

    if (sexp_exceptionp(res)) {
        String err_str = _sexp_to_string(sexp_exception_message(res));
        r_error.message = err_str;
        goto scheme_call_end;
    }

    if (sexp_fixnump(res)) {
        result = Variant(sexp_unbox_fixnum(res));
    }

    scheme_call_end:
    sexp_gc_release2(ctx);
    return result;
}


void ChibiSchemeBinder::scheme_free_instance(SchemeScriptInstance &p_target) {
    uint32_t inst_id = p_target.get_owner()->get_instance_id();
    if (!instance_id_ctx_map.has(inst_id)) {
        return;
    }
    sexp inst_ctx = instance_id_ctx_map.get(inst_id);
    sexp_destroy_context(inst_ctx);
}

