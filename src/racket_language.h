#ifndef RACKET_LANGUAGE_H
#define RACKET_LANGUAGE_H

#include "scheme_lang.h"
#include "racket_binder.h"

#include <godot_cpp/classes/mutex.hpp>
#include <godot_cpp/core/mutex_lock.hpp>
#include <godot_cpp/classes/script_language_extension.hpp>
#include <godot_cpp/classes/script.hpp>
#include <godot_cpp/variant/utility_functions.hpp>


using namespace godot;

class RacketLanguage : public ScriptLanguageExtension {
    GDCLASS(RacketLanguage, ScriptLanguageExtension)

    friend class RacketScript;
    friend class RacketScriptResourceLoader;

    static RacketLanguage* singleton;

    HashMap<String, Ref<Script>> scripts;
    RacketBinder *binder;
    Ref<Mutex> instance_lock;

protected:
    static void _bind_methods() { }
    // RacketScript* _do_create_script() const;

public:

    RacketLanguage();
    ~RacketLanguage();

    static RacketLanguage* get_singleton() {
        return singleton;
    }

    MutexLock get_instance_lock() {
        MutexLock ml(*get_singleton()->instance_lock.ptr());
        return ml;
    }


    RacketBinder* get_binder() {
        return binder;
    }

    // Internal code for use in extension module.
    static String s_get_extension();
    static String s_get_type();
    // Ref<Script> module_create_script() const;

    // Absolute file path of directory containing the library file for this plugin
    String get_install_dir();

    void _init() override;

	virtual String _get_name() const;
	virtual String _get_type() const;
	virtual String _get_extension() const;
	virtual void _finish();
	virtual PackedStringArray _get_reserved_words() const;
	virtual bool _is_control_flow_keyword(const String &keyword) const;
	virtual PackedStringArray _get_comment_delimiters() const;
	virtual PackedStringArray _get_string_delimiters() const;
	virtual Ref<Script> _make_template(const String &_template, const String &class_name, const String &base_class_name) const;
	virtual TypedArray<Dictionary> _get_built_in_templates(const StringName &object) const;
	virtual bool _is_using_templates();
	virtual Dictionary _validate(const String &script, const String &path, bool validate_functions, bool validate_errors, bool validate_warnings, bool validate_safe_lines) const;
	virtual String _validate_path(const String &path) const;
	virtual Object *_create_script() const;
	virtual bool _has_named_classes() const;
	virtual bool _supports_builtin_mode() const;
	virtual bool _supports_documentation() const;
	virtual bool _can_inherit_from_file() const;
	virtual int32_t _find_function(const String &class_name, const String &function_name) const;
	virtual String _make_function(const String &class_name, const String &function_name, const PackedStringArray &function_args) const;
	virtual Error _open_in_external_editor(const Ref<Script> &script, int32_t line, int32_t column);
	virtual bool _overrides_external_editor();
	virtual Dictionary _complete_code(const String &code, const String &path, Object *owner) const;
	virtual Dictionary _lookup_code(const String &code, const String &symbol, const String &path, Object *owner) const;
	virtual String _auto_indent_code(const String &code, int32_t from_line, int32_t to_line) const;
	virtual void _add_global_constant(const StringName &name, const Variant &value);
	virtual void _add_named_global_constant(const StringName &name, const Variant &value);
	virtual void _remove_named_global_constant(const StringName &name);
	virtual void _thread_enter();
	virtual void _thread_exit();
	virtual String _debug_get_error() const;
	virtual int32_t _debug_get_stack_level_count() const;
	virtual int32_t _debug_get_stack_level_line(int32_t level) const;
	virtual String _debug_get_stack_level_function(int32_t level) const;
	virtual Dictionary _debug_get_stack_level_locals(int32_t level, int32_t max_subitems, int32_t max_depth);
	virtual Dictionary _debug_get_stack_level_members(int32_t level, int32_t max_subitems, int32_t max_depth);
	virtual void *_debug_get_stack_level_instance(int32_t level);
	virtual Dictionary _debug_get_globals(int32_t max_subitems, int32_t max_depth);
	virtual String _debug_parse_stack_level_expression(int32_t level, const String &expression, int32_t max_subitems, int32_t max_depth);
	virtual TypedArray<Dictionary> _debug_get_current_stack_info();
	virtual void _reload_all_scripts();
	virtual void _reload_tool_script(const Ref<Script> &script, bool soft_reload);
	virtual PackedStringArray _get_recognized_extensions() const;
	virtual TypedArray<Dictionary> _get_public_functions() const;
	virtual Dictionary _get_public_constants() const;
	virtual TypedArray<Dictionary> _get_public_annotations() const;
	virtual void _profiling_start();
	virtual void _profiling_stop();
	virtual int32_t _profiling_get_accumulated_data(ScriptLanguageExtensionProfilingInfo *info_array, int32_t info_max);
	virtual int32_t _profiling_get_frame_data(ScriptLanguageExtensionProfilingInfo *info_array, int32_t info_max);
	virtual void _frame();
	virtual bool _handles_global_class_type(const String &type) const;
	virtual Dictionary _get_global_class_name(const String &path) const;

    // API implementation
    // void _thread_enter() override;
    // void _thread_exit() override;
    // void _frame() override;
    // String _get_name() const override;
    // void _init() override;
    // String _get_type() const override;
    // String _get_extension() const override;
    // void _finish() override;
    // PackedStringArray _get_reserved_words() const override;
    // bool _is_control_flow_keyword(const String& keyword) const override;
    // PackedStringArray _get_comment_delimiters() const override;
    // PackedStringArray _get_string_delimiters() const override;
    // TypedArray<Dictionary> _get_built_in_templates(const StringName &object) const override;
    // bool _is_using_templates() override;
    // Dictionary _validate(const String& script, const String& path, bool validate_functions, bool validate_errors,
    //                      bool validate_warnings, bool validate_safe_lines) const override;
    // String _validate_path(const String& path) const override;
    // Object* _create_script() const override;
    // bool _has_named_classes() const override;
    // bool _supports_builtin_mode() const override;
    // bool _supports_documentation() const override;
    // bool _can_inherit_from_file() const override;
    // int32_t _find_function(const String &class_name, const String &function_name) const override;
    // String _make_function(const String& class_name, const String& function_name,
    //                       const PackedStringArray& function_args) const override;
    // bool _overrides_external_editor() override;
    // Dictionary _complete_code(const String& code, const String& path, Object* owner) const override;
    // Dictionary _lookup_code(const String& code, const String& symbol, const String& path, Object* owner) const override;
    // String _auto_indent_code(const String &code, int32_t from_line, int32_t to_line) const override;
    // void _add_global_constant(const StringName& name, const Variant& value) override;
    // void _add_named_global_constant(const StringName& name, const Variant& value) override;
    // void _remove_named_global_constant(const StringName& name) override;
    // Dictionary _debug_get_globals(int32_t max_subitems, int32_t max_depth) override;
    // void _reload_all_scripts() override;
    // void _reload_tool_script(const godot::Ref<godot::Script>& script, bool soft_reload) override;
    // PackedStringArray _get_recognized_extensions() const override;
    // TypedArray<Dictionary> _get_public_functions() const override;
    // Dictionary _get_public_constants() const override;
    // TypedArray<Dictionary> _get_public_annotations() const override;
    // void _profiling_start() override;
    // void _profiling_stop() override;
    // bool _handles_global_class_type(const String& type) const override;
    // Dictionary _get_global_class_name(const String& path) const override;
    // godot::Ref<godot::Script> _make_template(const String& _template, const String& class_name, const String& base_class_name) const override;

    // godot::Ref<godot::ScriptLanguageThreadContextExtension> _create_thread_context() const override;

};

#endif // RACKET_LANGUAGE_H
