#include "./chibi_scheme_binder.h"


void ChibiSchemeBinder::initialize() {
    // sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
    // sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);
    // sexp_scheme_init();
}


void ChibiSchemeBinder::uninitialize() {
    // sexp_destroy_context(ctx);
}


void ChibiSchemeBinder::language_finalize() {

}


void ChibiSchemeBinder::frame() {

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
    return definition;
}


Variant ChibiSchemeBinder::scheme_call(SchemeScriptInstance &p_target, const Variant **p_args, int p_argcount, SchemeCallError &r_error) {
    sexp res = {};
    sexp_eval_string(ctx, "(+ 1 1)", -1, res);
    auto sint = sexp_unbox_fixnum(res);
    return Variant(sint);
}

void ChibiSchemeBinder::scheme_free(uint64_t object_id) {}

