#pragma once

#include "../scheme_binder.h"
#include "scheme_script_instance.h"
#include "chibi/eval.h"


class ChibiSchemeBinder : public SchemeBinder {
    sexp ctx;
	sexp stack;
	sexp env;

	HashMap<uint32_t, sexp> instance_id_ctx_map;

public:

	void initialize() override;
	void uninitialize() override;

    // Variant call_method(const SchemeObjectWrapper &p_object, const StringName &p_method, const Variant **p_args, int p_argcount, Callable::CallError &r_error) override;
	void scheme_initialize_instance(SchemeScriptInstance &p_target);
	int32_t scheme_get_member_line(SchemeScriptInstance &p_target, const StringName &member);
	GDClassDefinition scheme_create_definition(const SchemeScript &script);
	Variant scheme_call(SchemeScriptInstance &p_target, const String p_func_name, const Variant *p_args, int p_argcount, SchemeCallError &r_error);
	void scheme_free_instance(SchemeScriptInstance &p_target);

};
