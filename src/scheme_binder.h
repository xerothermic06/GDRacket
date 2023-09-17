#pragma once
#ifndef GDSCHEME_BINDER_H
#define GDSCHEME_BINDER_H

#include "godot_cpp/templates/hash_map.hpp"
#include "godot_cpp/templates/hash_set.hpp"
#include "godot_cpp/classes/thread.hpp"

// #include "scheme_gc_handler.h"
// #include "scheme_language.h"
// #include "scheme_script.h"
#include "scheme_error.h"
#include "util/scheme_classes.h"

class SchemeScriptInstance;


// Root interface for specific scheme implementations.
class SchemeBinder {
protected:
	// Path ==> JavaScript Class
	// HashMap<String, SchemeClassInfo> scheme_classes;
	// HashMap<int64_t, SchemeObjectWrapper> frame_callbacks;
	// HashSet<int64_t> canceled_frame_callbacks;
	// static String BINDING_SCRIPT_CONTENT;

public:
	enum EvalType {
		EVAL_TYPE_MODULE,
		EVAL_TYPE_GLOBAL,
	};

	SchemeBinder() {}
	virtual ~SchemeBinder(){};

	virtual void initialize() = 0;
	virtual void uninitialize() = 0;

	virtual void scheme_initialize_instance(SchemeScriptInstance &p_target) = 0;
	virtual int32_t scheme_get_member_line(SchemeScriptInstance &p_target, const StringName &member) = 0;
	virtual GDClassDefinition scheme_create_definition(const SchemeScript &script) = 0;
	virtual Variant scheme_call(SchemeScriptInstance &p_target, const String p_func_name, const Variant **p_args, int p_argcount, SchemeCallError* r_error) = 0;
	virtual bool set(SchemeScriptInstance &p_target, const StringName &p_name, const Variant &p_value) = 0;
	virtual bool get(const SchemeScriptInstance &p_target, const StringName, Variant &r_ret) const = 0;
	virtual bool has_method(const SchemeScriptInstance &p_target, const StringName &p_method) const = 0;
	virtual void scheme_free_instance(SchemeScriptInstance &p_target) = 0;

#ifdef TOOLS_ENABLED
	virtual const Dictionary &get_modified_api() const;
#endif
};

#endif // GDSCHEME_BINDER_H
