#pragma once

#include "../scheme_binder.h"
#include "scheme_script_instance.h"
#include "chibi/eval.h"


class ChibiSchemeBinder : public SchemeBinder {
    sexp ctx;

public:
	void initialize() override;
	void uninitialize() override;
	void language_finalize() override;
	void frame() override;

    // Variant call_method(const SchemeObjectWrapper &p_object, const StringName &p_method, const Variant **p_args, int p_argcount, Callable::CallError &r_error) override;
	int32_t scheme_get_member_line(SchemeScriptInstance &p_target, const StringName &member);
	GDClassDefinition scheme_create_definition(const SchemeScript &script);
	Variant scheme_call(SchemeScriptInstance &p_target, const Variant **p_args, int p_argcount, SchemeCallError &r_error);
	void scheme_free(uint64_t object_id);

};
