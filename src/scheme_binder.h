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

// typedef SchemeObjectWrapper JSMethodInfo;

// struct SchemeProperyInfo : public PropertyInfo {
// 	Variant default_value;
// };

// struct SchemeError {
// 	int line;
// 	int column;
// 	String message;
// 	String file;
// 	std::vector<String> stack;
// };

// struct BasicSchemeClassInfo {
// 	bool tool;
// 	StringName class_name;
// 	String icon_path;
// 	const ClassDB::ClassInfo *native_class;
// 	HashMap<StringName, MethodInfo> signals;
// 	HashMap<StringName, MethodInfo> methods;
// 	HashMap<StringName, SchemeProperyInfo> properties;
// };

// struct SchemeClassInfo : public BasicSchemeClassInfo {
// 	SchemeObjectWrapper constructor;
// 	SchemeObjectWrapper prototype;
// };

// struct GlobalNumberConstant {
// 	StringName name;
// 	double_t value;
// };

// struct SchemeStackInfo {
// 	int line;
// 	String file;
// 	String function;
// };

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

	// virtual SchemeBinder *get_context_binder(void *p_context) = 0;
	// virtual String get_thread_id() const = 0;

	// virtual void clear_classes() { scheme_classes.clear(); }

	// entry, initialization, etc
	virtual void initialize() = 0;
	// exit, de-initialization, etc
	virtual void uninitialize() = 0;
	virtual void language_finalize() = 0;
	virtual void frame() = 0;

	// virtual JavaScriptGCHandler *alloc_object_binding_data(Object *p_object) = 0;
	// virtual void free_object_binding_data(JavaScriptGCHandler *p_gc_handle) = 0;
	// virtual void godot_refcount_incremented(JavaScriptGCHandler *p_gc_handle) = 0;
	// virtual bool godot_refcount_decremented(JavaScriptGCHandler *p_gc_handle) = 0;

	// virtual Error eval_string(const String &p_source, EvalType type, const String &p_path, JavaScriptGCHandler &r_ret) = 0;
	// virtual Error safe_eval_text(const String &p_source, EvalType type, const String &p_path, String &r_error, JavaScriptGCHandler &r_ret) = 0;
	// virtual String error_to_string(const SchemeError &p_error) = 0;
	// virtual Error get_stacks(List<SchemeStackInfo> &r_stacks) = 0;
	// virtual String get_backtrace_message(const List<SchemeStackInfo> &stacks) = 0;

	// virtual Error compile_to_bytecode(const String &p_code, const String &p_file, Vector<uint8_t> &r_bytecode) = 0;
	// virtual Error load_bytecode(const Vector<uint8_t> &p_bytecode, const String &p_file, JavaScriptGCHandler *r_module) = 0;
	// virtual const SchemeClassInfo *parse_javascript_class(const String &p_code, const String &p_path, bool ignore_cacehe, SchemeError *r_error) = 0;
	// virtual const SchemeClassInfo *parse_javascript_class(const Vector<uint8_t> &p_bytecode, const String &p_path, bool ignore_cacehe, SchemeError *r_error) = 0;

	// virtual JavaScriptGCHandler create_js_instance_for_godot_object(const SchemeClassInfo *p_class, Object *p_object) = 0;
	// virtual bool get_instance_property(const JavaScriptGCHandler &p_object, const StringName &p_name, Variant &r_ret) = 0;
	// virtual bool set_instance_property(const JavaScriptGCHandler &p_object, const StringName &p_name, const Variant &p_value) = 0;
	// virtual bool has_method(const JavaScriptGCHandler &p_object, const StringName &p_name) = 0;
	// virtual bool has_signal(const SchemeClassInfo *p_class, const StringName &p_signal) = 0;
	// virtual bool validate(const String &p_code, const String &p_path, SchemeError *r_error) = 0;

	// virtual Variant call_method(const JavaScriptGCHandler &p_object, const StringName &p_method, const Variant **p_args, int p_argcount, Callable::CallError &r_error) = 0;

	virtual int32_t scheme_get_member_line(SchemeScriptInstance &p_target, const StringName &member) = 0;
	virtual GDClassDefinition scheme_create_definition(const SchemeScript &script) = 0;
	virtual Variant scheme_call(SchemeScriptInstance &p_target, const Variant **p_args, int p_argcount, SchemeCallError &r_error) = 0;
	virtual void scheme_free(uint64_t object_id) = 0;

#ifdef TOOLS_ENABLED
	virtual const Dictionary &get_modified_api() const;
#endif
};

#endif // GDSCHEME_BINDER_H
