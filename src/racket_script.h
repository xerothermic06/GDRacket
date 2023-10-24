#ifndef RACKET_SCRIPT_H
#define RACKET_SCRIPT_H

#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/core/defs.hpp>
#include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/script_extension.hpp>
#include <godot_cpp/classes/resource_format_loader.hpp>
#include <godot_cpp/classes/resource_format_saver.hpp>
#include <godot_cpp/templates/list.hpp>

#include "racket_language.h"
#include "racket_script_instance.h"


using namespace godot;

class GDClassDefinition;

class RacketScript : public ScriptExtension {
	GDCLASS(RacketScript, ScriptExtension)

	RacketLanguage* language = nullptr;

    Ref<Mutex> instance_lock;
	String source_code;
	GDClassDefinition class_definition;
	HashMap<uint64_t, RacketScriptInstance*> instances;
	bool is_valid = false;

protected:
	static void _bind_methods() { }

public:
	RacketScript();
	~RacketScript();

    MutexLock get_instance_lock() {
		auto mut_ptr = *instance_lock.ptr();
        MutexLock ml(mut_ptr);
        return ml;
    }

	Error load();

	bool _can_instantiate() const override;
	bool _has_source_code() const override;
	bool _has_method(const StringName& method) const override;
	bool _is_tool() const override;
	bool _is_valid() const override;
	bool _has_script_signal(const StringName& signal) const override;
	bool _is_placeholder_fallback_enabled() const override;
	bool _has_property_default_value(const StringName& name) const override;

	TypedArray<Dictionary> _get_script_signal_list() const override;
	TypedArray<Dictionary> _get_script_method_list() const override;
	TypedArray<Dictionary> _get_script_property_list() const override;
	TypedArray<StringName> _get_members() const override;
	TypedArray<Dictionary> _get_documentation() const override;

	Dictionary _get_constants() const override;

	Ref<Script> _get_base_script() const override;

	ScriptLanguage* _get_language() const override;

	StringName _get_instance_base_type() const override;
	String _get_source_code() const override;

	void _set_source_code(const String& code) override;
	void _update_exports() override;
	void* _instance_create(Object* for_object) const override;

	bool _editor_can_reload_from_file() override;
	StringName _get_global_name() const override;
	bool _inherits_script(const Ref<Script> &script) const override;
	bool _instance_has(Object *object) const override;

	Error _reload(bool keep_state) override;
	Dictionary _get_method_info(const StringName &method) const override;
	Variant _get_property_default_value(const StringName &property) const override;
	int32_t _get_member_line(const StringName &member) const override;
	Variant _get_rpc_config() const override;
};


// SchemeModule ////////////////////////////////////////////////////////////////
class SchemeModule : public Resource {
	GDCLASS(SchemeModule, Resource)

	String source_code;

protected:
	static void _bind_methods() {}

public:
	String get_source_code() { return source_code; }
	// TODO: track exports or other symbols for editor integration?

};


// ResourceLoader //////////////////////////////////////////////////////////////

class RacketScriptResourceLoader : public ResourceFormatLoader {
	GDCLASS(RacketScriptResourceLoader, ResourceFormatLoader)

	friend void initialize_racket_module(ModuleInitializationLevel p_level);
	RacketLanguage* language = nullptr;
	// Ref<RacketLanguage> language;

	Variant _load_script(const String &path) const;
	Variant _load_module(const String &path) const;

protected:
	static void _bind_methods() { }

public:
	RacketScriptResourceLoader();
	~RacketScriptResourceLoader();
	PackedStringArray _get_recognized_extensions() const override;
	bool _handles_type(const StringName& p_type) const override;
	String _get_resource_type(const String& path) const override;
	PackedStringArray _get_dependencies(const String& path, bool add_types) const override;
	Variant _load(const String &path, const String &original_path, bool use_sub_threads, int32_t cache_mode) const override;

	bool _recognize_path(const String &path, const StringName &type) const;
	String _get_resource_script_class(const String &path) const;

};


// ResourceSaver ///////////////////////////////////////////////////////////////

class RacketScriptResourceSaver : public ResourceFormatSaver {
	GDCLASS(RacketScriptResourceSaver, ResourceFormatSaver)

protected:
	static void _bind_methods() { }

public:
	RacketScriptResourceSaver();
	~RacketScriptResourceSaver();
	Error _save(const Ref<Resource> &resource, const String &path, uint32_t flags) override;
	bool _recognize(const Ref<Resource>& resource) const override;
	PackedStringArray _get_recognized_extensions(const Ref<Resource>& resource) const override;
};

#endif // RACKET_SCRIPT_H
