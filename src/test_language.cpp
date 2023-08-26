
#include "test_language.h"


void TestLanguage::_thread_enter() {
}

void TestLanguage::_thread_exit() {
}

void TestLanguage::_frame() {
}

String TestLanguage::_get_name() const {
    return "test";
}

// String TestLanguage::module_get_type_constant() {
    // return "Script";
// }

String TestLanguage::_get_type() const {
    // return module_get_type_constant();
    return "TestLanguage";
}

// String TestLanguage::module_get_extension_constant() {
//     return "scm";
// }

String TestLanguage::_get_extension() const {
    // return module_get_extension_constant();
    return "test";
}

void TestLanguage::_finish() {
}

PackedStringArray TestLanguage::_get_reserved_words() const {
    return PackedStringArray();
}

bool TestLanguage::_is_control_flow_keyword(const String& keyword) const {
    return false;
}

PackedStringArray TestLanguage::_get_comment_delimiters() const {
    // return PackedStringArray(SCHEME_COMMENT_DELIMITERS);
    return PackedStringArray();
}

PackedStringArray TestLanguage::_get_string_delimiters() const {
    // return PackedStringArray(SCHEME_STRING_DELIMITERS);
    return PackedStringArray();
}

TypedArray<Dictionary> TestLanguage::_get_built_in_templates(const StringName &object) const {
    return Array();
}

bool TestLanguage::_is_using_templates() {
    return false;
}

Dictionary TestLanguage::_validate(const String& script, const String& path, bool validate_functions,
                                          bool validate_errors, bool validate_warnings,
                                          bool validate_safe_lines) const {
    Dictionary validation;
    validation["valid"] = true;
    return validation;
}

String TestLanguage::_validate_path(const String& path) const {
    return "";
}

// Script* TestLanguage::_do_create_script() const {
    // SchemeScript* created = memnew(SchemeScript());
    // created->language = const_cast<TestLanguage*>(this);
    // return created;
    // return nullptr;
// }

Object* TestLanguage::_create_script() const {
    // return _do_create_script();
    return nullptr;
}

bool TestLanguage::_has_named_classes() const {
    return true;
}

bool TestLanguage::_supports_builtin_mode() const {
    return false;
}

bool TestLanguage::_supports_documentation() const {
    return true;
}

bool TestLanguage::_can_inherit_from_file() const {
    return true;
}

int32_t TestLanguage::_find_function(const String &class_name, const String &function_name) const {
    return 0;
}

String TestLanguage::_make_function(const String& class_name, const String& function_name,
                                           const PackedStringArray& function_args) const {
    return "";
}

bool TestLanguage::_overrides_external_editor() {
    return false;
}

Dictionary TestLanguage::_complete_code(const String& code, const String& path, Object* owner) const {
    return Dictionary();
}

Dictionary TestLanguage::_lookup_code(const String& code, const String& symbol, const String& path,
                                             Object* owner) const {
    return Dictionary();
}

String TestLanguage::_auto_indent_code(const String& code, int32_t from_line, int32_t to_line) const {
    return "";
}

void TestLanguage::_add_global_constant(const StringName& name, const Variant& value) {
}

void TestLanguage::_add_named_global_constant(const StringName& name, const Variant& value) {
}

void TestLanguage::_remove_named_global_constant(const StringName& name) {
}

Dictionary TestLanguage::_debug_get_globals(int32_t max_subitems, int32_t max_depth) {
    return Dictionary();
}

void TestLanguage::_reload_all_scripts() {
}

void TestLanguage::_reload_tool_script(const Ref<Script>& script, bool soft_reload) {
}

PackedStringArray TestLanguage::_get_recognized_extensions() const {
    PackedStringArray extensions;
    extensions.append(_get_extension());
    return extensions;
}

TypedArray<Dictionary> TestLanguage::_get_public_functions() const {
    return Array();
}

Dictionary TestLanguage::_get_public_constants() const {
    return Dictionary();
}

TypedArray<Dictionary> TestLanguage::_get_public_annotations() const {
    return Array();
}

void TestLanguage::_profiling_start() {
}

void TestLanguage::_profiling_stop() {
}

bool TestLanguage::_handles_global_class_type(const String& type) const {
    return false;
}

Dictionary TestLanguage::_get_global_class_name(const String& path) const {
    return Dictionary();
}

// Ref<Script> TestLanguage::module_create_script() const {
//     SchemeScript* created = _do_create_script();
//     Ref<Script> ret = Ref<Script>(created);
//     // // // https://github.com/godotengine/godot-cpp/issues/652
//     // // ret->reference();
//     return ret;
//     // return nullptr;
// }

Ref<Script> TestLanguage::_make_template(const String& _template, const String& class_name,
                                                const String& base_class_name) const {
    // return module_create_script();
    return nullptr;
}

Error TestLanguage::_open_in_external_editor(const Ref<Script> &script, int32_t line, int32_t column) {
    return Error::OK;
}
String TestLanguage::_debug_get_error() const {
    return "";
}
int32_t TestLanguage::_debug_get_stack_level_count() const {
    return 0;
}
int32_t TestLanguage::_debug_get_stack_level_line(int32_t level) const {
    return 0;
}
String TestLanguage::_debug_get_stack_level_function(int32_t level) const {
    return "";
}
Dictionary TestLanguage::_debug_get_stack_level_locals(int32_t level, int32_t max_subitems, int32_t max_depth) {
    return Dictionary();
}
Dictionary TestLanguage::_debug_get_stack_level_members(int32_t level, int32_t max_subitems, int32_t max_depth) {
    return Dictionary();
}
void *TestLanguage::_debug_get_stack_level_instance(int32_t level) {
    return nullptr;
}
String TestLanguage::_debug_parse_stack_level_expression(int32_t level, const String &expression, int32_t max_subitems, int32_t max_depth) {
    return "";
}
int32_t TestLanguage::_profiling_get_accumulated_data(ScriptLanguageExtensionProfilingInfo *info_array, int32_t info_max) {
    return 0;
}
int32_t TestLanguage::_profiling_get_frame_data(ScriptLanguageExtensionProfilingInfo *info_array, int32_t info_max) {
    return 0;
}
TypedArray<Dictionary> TestLanguage::_debug_get_current_stack_info() {
    return TypedArray<Dictionary>();
}
