#pragma once
#ifndef GDSCHEME_BINDER_H
#define GDSCHEME_BINDER_H

#include "godot_cpp/templates/hash_map.hpp"
#include "godot_cpp/templates/hash_set.hpp"
#include "godot_cpp/classes/thread.hpp"

#include "scheme_error.h"
#include "util/scheme_classes.h"

class SchemeScriptInstance;


// Root interface for specific scheme implementations. This class is responsible
// for managing the lifetime of the given Scheme runtime, managing instances,
// and exporting script class definitions.
class SchemeBinder {
protected:

public:
	enum EvalType {
		EVAL_TYPE_MODULE,
		EVAL_TYPE_GLOBAL,
	};

	// Initialization for individual script instances for which classes have already been defined and registered.
	virtual Error initialize_instance(SchemeScriptInstance &p_target) = 0;
	// TODO: is this is needed?
	virtual int32_t get_member_line(SchemeScriptInstance &p_target, const StringName &member) = 0;

	// Initialization for a script class definition from a provided script resource.
	virtual Error create_definition(const SchemeScript &script, GDClassDefinition& def) = 0;
	// Gets a script class definition for a given script resource if it exists.
    virtual GDClassDefinition* get_definition(const SchemeScript &script) = 0;
	virtual void delete_definition(const SchemeScript &p_script) = 0;

	// Calls a method on a script instance.
	virtual Variant call(SchemeScriptInstance &p_target, const String p_func_name, const Variant **p_args, int p_argcount, SchemeCallError* r_error) = 0;
	// Sets a property from a script instance.
	virtual bool set(SchemeScriptInstance &p_target, const StringName &p_name, const Variant &p_value) = 0;
	// Gets a property from a script instance.
	virtual bool get(const SchemeScriptInstance &p_target, const StringName, Variant *r_ret) const = 0;
	// Returns whether or not a script instance has a property with the given name.
	virtual bool has_method(const SchemeScriptInstance &p_target, const StringName &p_method) const = 0;
	// Script instance finalization.
	virtual void free_instance(SchemeScriptInstance &p_target) = 0;

#ifdef TOOLS_ENABLED
	virtual const Dictionary &get_modified_api() const;
#endif
};

#endif // GDSCHEME_BINDER_H
