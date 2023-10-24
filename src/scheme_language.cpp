#include "godot_cpp/classes/project_settings.hpp"

#include "scheme_language.h"
#include "scheme_script.h"
#include "binder/racket_binder.h"

// TODO: loads of copy-pasted stuff here from other language plugins. This
// merits a lot of re-work and more judicious inclusion

// TODO: MSVC seems to not want to compile these in UTF-8
#define INIT_BANNER_MSG "λλλλλλλλλλλλλλλλλλλλ\nGDRacket initialized\nλλλλλλλλλλλλλλλλλλλλ"

SchemeLanguage* SchemeLanguage::singleton;

SchemeLanguage::SchemeLanguage() {
    singleton = this;
    binder = RacketBinder::get_singleton(); //memnew(RacketBinder());
    instance_lock.instantiate();
}


SchemeLanguage::~SchemeLanguage() {
    delete binder;
    singleton = nullptr;
}


//TODO: Change this to do it right obv
String SchemeLanguage::get_install_dir() {
    return ProjectSettings::get_singleton()->globalize_path("res://scheme");
}


void SchemeLanguage::_thread_enter() {
}


void SchemeLanguage::_thread_exit() {
}


void SchemeLanguage::_frame() {
}


String SchemeLanguage::_get_name() const {
    return "scheme";
}


void SchemeLanguage::_init() {
    UtilityFunctions::print(INIT_BANNER_MSG);
}


String SchemeLanguage::s_get_type() {
    return "SchemeScript";
}


String SchemeLanguage::_get_type() const {
    return s_get_type();
}


String SchemeLanguage::s_get_extension() {
    return "rkt";
}


String SchemeLanguage::_get_extension() const {
    return s_get_extension();
}


void SchemeLanguage::_finish() {
}


PackedStringArray SchemeLanguage::_get_reserved_words() const {
    return PackedStringArray();
}


bool SchemeLanguage::_is_control_flow_keyword(const String& keyword) const {
    return false;
}


PackedStringArray SchemeLanguage::_get_comment_delimiters() const {
    auto ret = PackedStringArray();
    ret.append(";");
    ret.append("#|");
    ret.append("|#");
    return ret;
}


PackedStringArray SchemeLanguage::_get_string_delimiters() const {
    auto ret = PackedStringArray();
    ret.append("\"");
    return ret;
}


TypedArray<Dictionary> SchemeLanguage::_get_built_in_templates(const StringName &object) const {
    return Array();
}


bool SchemeLanguage::_is_using_templates() {
    return false;
}


Dictionary SchemeLanguage::_validate(const String& script, const String& path, bool validate_functions,
                                          bool validate_errors, bool validate_warnings,
                                          bool validate_safe_lines) const {
    Dictionary validation;
    validation["valid"] = true;
    return validation;
}


String SchemeLanguage::_validate_path(const String& path) const {
    return "";
}


Object* SchemeLanguage::_create_script() const {
    SchemeScript* created = memnew(SchemeScript());
    created->language = const_cast<SchemeLanguage*>(this);
    return created;
}


bool SchemeLanguage::_has_named_classes() const {
    return true;
}


bool SchemeLanguage::_supports_builtin_mode() const {
    return false;
}


bool SchemeLanguage::_supports_documentation() const {
    return true;
}


bool SchemeLanguage::_can_inherit_from_file() const {
    return true;
}


int32_t SchemeLanguage::_find_function(const String &class_name, const String &function_name) const {
    return 0;
}


String SchemeLanguage::_make_function(const String& class_name, const String& function_name,
                                           const PackedStringArray& function_args) const {
    return "";
}


bool SchemeLanguage::_overrides_external_editor() {
    return false;
}


Dictionary SchemeLanguage::_complete_code(const String& code, const String& path, Object* owner) const {
    return Dictionary();
}


Dictionary SchemeLanguage::_lookup_code(const String& code, const String& symbol, const String& path,
                                             Object* owner) const {
    return Dictionary();
}


String SchemeLanguage::_auto_indent_code(const String& code, int32_t from_line, int32_t to_line) const {
    return "";
}


void SchemeLanguage::_add_global_constant(const StringName& name, const Variant& value) {
}


void SchemeLanguage::_add_named_global_constant(const StringName& name, const Variant& value) {
}


void SchemeLanguage::_remove_named_global_constant(const StringName& name) {
}


Dictionary SchemeLanguage::_debug_get_globals(int32_t max_subitems, int32_t max_depth) {
    return Dictionary();
}


void SchemeLanguage::_reload_all_scripts() {
}


void SchemeLanguage::_reload_tool_script(const Ref<Script>& script, bool soft_reload) {
}


PackedStringArray SchemeLanguage::_get_recognized_extensions() const {
    PackedStringArray extensions;
    extensions.append(_get_extension());
    return extensions;
}


TypedArray<Dictionary> SchemeLanguage::_get_public_functions() const {
    return Array();
}


Dictionary SchemeLanguage::_get_public_constants() const {
    return Dictionary();
}


TypedArray<Dictionary> SchemeLanguage::_get_public_annotations() const {
    return Array();
}


void SchemeLanguage::_profiling_start() {
}


void SchemeLanguage::_profiling_stop() {
}


bool SchemeLanguage::_handles_global_class_type(const String& type) const {
    return false;
}


Dictionary SchemeLanguage::_get_global_class_name(const String& path) const {
    return Dictionary();
}


Ref<Script> SchemeLanguage::_make_template(const String& _template, const String& class_name, const String& base_class_name) const {
    return Ref<Script>(_create_script());
}


Error SchemeLanguage::_open_in_external_editor(const Ref<Script> &script, int32_t line, int32_t column) {
    return Error::OK;
}


String SchemeLanguage::_debug_get_error() const {
    return "";
}


int32_t SchemeLanguage::_debug_get_stack_level_count() const {
    return 0;
}


int32_t SchemeLanguage::_debug_get_stack_level_line(int32_t level) const {
    return 0;
}


String SchemeLanguage::_debug_get_stack_level_function(int32_t level) const {
    return "";
}


Dictionary SchemeLanguage::_debug_get_stack_level_locals(int32_t level, int32_t max_subitems, int32_t max_depth) {
    return Dictionary();
}


Dictionary SchemeLanguage::_debug_get_stack_level_members(int32_t level, int32_t max_subitems, int32_t max_depth) {
    return Dictionary();
}


void *SchemeLanguage::_debug_get_stack_level_instance(int32_t level) {
    return nullptr;
}


String SchemeLanguage::_debug_parse_stack_level_expression(int32_t level, const String &expression, int32_t max_subitems, int32_t max_depth) {
    return "";
}


int32_t SchemeLanguage::_profiling_get_accumulated_data(ScriptLanguageExtensionProfilingInfo *info_array, int32_t info_max) {
    return 0;
}


int32_t SchemeLanguage::_profiling_get_frame_data(ScriptLanguageExtensionProfilingInfo *info_array, int32_t info_max) {
    return 0;
}


TypedArray<Dictionary> SchemeLanguage::_debug_get_current_stack_info() {
    return TypedArray<Dictionary>();
}

