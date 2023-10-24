#include "godot_cpp/classes/project_settings.hpp"

#include "racket_language.h"
#include "racket_script.h"
#include "binder/racket_bc_binder.h"

// TODO: loads of copy-pasted stuff here from other language plugins. This
// merits a lot of re-work and more judicious inclusion

// TODO: MSVC seems to not want to compile these in UTF-8
#define INIT_BANNER_MSG "λλλλλλλλλλλλλλλλλλλλ\nGDRacket initialized\nλλλλλλλλλλλλλλλλλλλλ"

RacketLanguage* RacketLanguage::singleton;

RacketLanguage::RacketLanguage() {
    singleton = this;
    binder = RacketBCBinder::get_singleton();
    instance_lock.instantiate();
}


RacketLanguage::~RacketLanguage() {
    delete binder;
    singleton = nullptr;
}


//TODO: Change this to allow configurable install path
String RacketLanguage::get_install_dir() {
    return ProjectSettings::get_singleton()->globalize_path("res://racket");
}


void RacketLanguage::_thread_enter() {
}


void RacketLanguage::_thread_exit() {
}


void RacketLanguage::_frame() {
}


String RacketLanguage::_get_name() const {
    return "Racket";
}


void RacketLanguage::_init() {
    UtilityFunctions::print(INIT_BANNER_MSG);
}


String RacketLanguage::s_get_type() {
    return "RacketScript";
}


String RacketLanguage::_get_type() const {
    return s_get_type();
}


String RacketLanguage::s_get_extension() {
    return "rkt";
}


String RacketLanguage::_get_extension() const {
    return s_get_extension();
}


void RacketLanguage::_finish() {
}


PackedStringArray RacketLanguage::_get_reserved_words() const {
    return PackedStringArray();
}


bool RacketLanguage::_is_control_flow_keyword(const String& keyword) const {
    return false;
}


PackedStringArray RacketLanguage::_get_comment_delimiters() const {
    auto ret = PackedStringArray();
    ret.append(";");
    ret.append("#|");
    ret.append("|#");
    return ret;
}


PackedStringArray RacketLanguage::_get_string_delimiters() const {
    auto ret = PackedStringArray();
    ret.append("\"");
    return ret;
}


TypedArray<Dictionary> RacketLanguage::_get_built_in_templates(const StringName &object) const {
    return Array();
}


bool RacketLanguage::_is_using_templates() {
    return false;
}


Dictionary RacketLanguage::_validate(const String& script, const String& path, bool validate_functions,
                                          bool validate_errors, bool validate_warnings,
                                          bool validate_safe_lines) const {
    Dictionary validation;
    validation["valid"] = true;
    return validation;
}


String RacketLanguage::_validate_path(const String& path) const {
    return "";
}


Object* RacketLanguage::_create_script() const {
    RacketScript* created = memnew(RacketScript());
    created->language = const_cast<RacketLanguage*>(this);
    return created;
}


bool RacketLanguage::_has_named_classes() const {
    return true;
}


bool RacketLanguage::_supports_builtin_mode() const {
    return false;
}


bool RacketLanguage::_supports_documentation() const {
    return true;
}


bool RacketLanguage::_can_inherit_from_file() const {
    return true;
}


int32_t RacketLanguage::_find_function(const String &class_name, const String &function_name) const {
    return 0;
}


String RacketLanguage::_make_function(const String& class_name, const String& function_name,
                                           const PackedStringArray& function_args) const {
    return "";
}


bool RacketLanguage::_overrides_external_editor() {
    return false;
}


Dictionary RacketLanguage::_complete_code(const String& code, const String& path, Object* owner) const {
    return Dictionary();
}


Dictionary RacketLanguage::_lookup_code(const String& code, const String& symbol, const String& path,
                                             Object* owner) const {
    return Dictionary();
}


String RacketLanguage::_auto_indent_code(const String& code, int32_t from_line, int32_t to_line) const {
    return "";
}


void RacketLanguage::_add_global_constant(const StringName& name, const Variant& value) {
}


void RacketLanguage::_add_named_global_constant(const StringName& name, const Variant& value) {
}


void RacketLanguage::_remove_named_global_constant(const StringName& name) {
}


Dictionary RacketLanguage::_debug_get_globals(int32_t max_subitems, int32_t max_depth) {
    return Dictionary();
}


void RacketLanguage::_reload_all_scripts() {
}


void RacketLanguage::_reload_tool_script(const Ref<Script>& script, bool soft_reload) {
}


PackedStringArray RacketLanguage::_get_recognized_extensions() const {
    PackedStringArray extensions;
    extensions.append(_get_extension());
    return extensions;
}


TypedArray<Dictionary> RacketLanguage::_get_public_functions() const {
    return Array();
}


Dictionary RacketLanguage::_get_public_constants() const {
    return Dictionary();
}


TypedArray<Dictionary> RacketLanguage::_get_public_annotations() const {
    return Array();
}


void RacketLanguage::_profiling_start() {
}


void RacketLanguage::_profiling_stop() {
}


bool RacketLanguage::_handles_global_class_type(const String& type) const {
    return false;
}


Dictionary RacketLanguage::_get_global_class_name(const String& path) const {
    return Dictionary();
}


Ref<Script> RacketLanguage::_make_template(const String& _template, const String& class_name, const String& base_class_name) const {
    return Ref<Script>(_create_script());
}


Error RacketLanguage::_open_in_external_editor(const Ref<Script> &script, int32_t line, int32_t column) {
    return Error::OK;
}


String RacketLanguage::_debug_get_error() const {
    return "";
}


int32_t RacketLanguage::_debug_get_stack_level_count() const {
    return 0;
}


int32_t RacketLanguage::_debug_get_stack_level_line(int32_t level) const {
    return 0;
}


String RacketLanguage::_debug_get_stack_level_function(int32_t level) const {
    return "";
}


Dictionary RacketLanguage::_debug_get_stack_level_locals(int32_t level, int32_t max_subitems, int32_t max_depth) {
    return Dictionary();
}


Dictionary RacketLanguage::_debug_get_stack_level_members(int32_t level, int32_t max_subitems, int32_t max_depth) {
    return Dictionary();
}


void *RacketLanguage::_debug_get_stack_level_instance(int32_t level) {
    return nullptr;
}


String RacketLanguage::_debug_parse_stack_level_expression(int32_t level, const String &expression, int32_t max_subitems, int32_t max_depth) {
    return "";
}


int32_t RacketLanguage::_profiling_get_accumulated_data(ScriptLanguageExtensionProfilingInfo *info_array, int32_t info_max) {
    return 0;
}


int32_t RacketLanguage::_profiling_get_frame_data(ScriptLanguageExtensionProfilingInfo *info_array, int32_t info_max) {
    return 0;
}


TypedArray<Dictionary> RacketLanguage::_debug_get_current_stack_info() {
    return TypedArray<Dictionary>();
}

