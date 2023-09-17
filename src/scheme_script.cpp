// #include <fstream>
#include "godot_cpp/classes/file_access.hpp"


#include "scheme_language.h"
// #include "scheme_script_instance.h"
#include "scheme_script.h"

// #include <godot_cpp/classes/script_language_thread_context_extension.hpp>
// #include <godot_cpp/classes/file.hpp>

SchemeScript::SchemeScript() {
    language = SchemeLanguage::get_singleton();
}


SchemeScript::~SchemeScript() {
}


Error SchemeScript::load() {
    class_definition = language->binder->scheme_create_definition(*this);
    return Error::OK;
}


bool SchemeScript::_can_instantiate() const {
    return true;
}


bool SchemeScript::_has_source_code() const {
    return !source_code.is_empty();
}


bool SchemeScript::_has_method(const StringName& method) const {
    return class_definition.methods.has(method);
}


bool SchemeScript::_is_tool() const {
    return false;
}


bool SchemeScript::_is_valid() const {
    return true;
}


ScriptLanguage* SchemeScript::_get_language() const {
    return language;
}


bool SchemeScript::_has_script_signal(const StringName& signal) const {
    // TODO: check for equivalent hyphentated sexpr name
    return class_definition.signals.has(signal);
}


TypedArray<Dictionary> SchemeScript::_get_script_signal_list() const {
    auto signals = Array();
    for (const KeyValue<StringName, GDMethod> kv : class_definition.signals) {
        signals.push_back(Dictionary(kv.value));
    }
    return signals;
}


TypedArray<Dictionary> SchemeScript::_get_script_method_list() const {
    auto methods = Array();
    for (const KeyValue<StringName, GDMethod> kv : class_definition.methods) {
        methods.push_back(Dictionary(kv.value));
    }
    return methods;
}


TypedArray<Dictionary> SchemeScript::_get_script_property_list() const {
    TypedArray<Dictionary> properties;
    const Script *cur_script = this;
    while (cur_script) {
        for (int i = 0; i < class_definition.properties.size(); i++) {
            auto prop = class_definition.properties[i];
            properties.push_front(Dictionary(prop.property));
        }
        cur_script = (cur_script->get_base_script().ptr());
    }
    return properties;
}


Dictionary SchemeScript::_get_constants() const {
    return Dictionary();
}


TypedArray<StringName> SchemeScript::_get_members() const {
    TypedArray<StringName> members;

    for (const GDClassProperty &prop : class_definition.properties)
        members.push_back(prop.property.name);

    return members;
    // return Array();
}


bool SchemeScript::_is_placeholder_fallback_enabled() const {
    return false;
}


Ref<Script> SchemeScript::_get_base_script() const {
    return Ref<Script>();
}


StringName SchemeScript::_get_instance_base_type() const {
    // return StringName("Object");
    StringName extends = StringName(class_definition.extends);

    if (extends != StringName() && SchemeClassDB::class_exists(extends)) {
        return extends;
    }

    auto base_script = class_definition.base_script;
    // if (base_script != nullptr && base_script->_is_valid())
    if (class_definition.has_base_script()) {
        return base_script->get_instance_base_type();
    }
    return StringName();
}


String SchemeScript::_get_source_code() const {
    return source_code; //String("This language does not use source code");
}


void SchemeScript::_set_source_code(const String& code) {
    source_code = code;
}


void SchemeScript::_update_exports() {
    // nothing to do
}


void* SchemeScript::_instance_create(Object* for_object) const {
    return SchemeScriptInstance::create_instance(this, for_object);
}


TypedArray<Dictionary> SchemeScript::_get_documentation() const {
    return Array();
}


bool SchemeScript::_has_property_default_value(const StringName&) const {
    return false;
}


bool SchemeScript::_editor_can_reload_from_file() {
    return true;
}


// void SchemeScript::_placeholder_erased(void *placeholder) {

// }


StringName SchemeScript::_get_global_name() const {
    return get_name();
}


bool SchemeScript::_inherits_script(const Ref<Script> &p_script) const {
    Ref<Script> script = p_script;
    if (script.is_null())
        return false;

    Ref<Script> cur_script = this;

    while (cur_script.is_valid()) {
        if (cur_script == script.ptr())
            return true;

        cur_script = cur_script->get_base_script();
    }

    return false;
}


// void* SchemeScript::_placeholder_instance_create(Object *for_object) const {
//     return nullptr;
// }


bool SchemeScript::_instance_has(Object *object) const {
    // auto lock(*SchemeLanguage::singleton->instance_lock.ptr());
    SchemeLanguage::get_singleton()->get_instance_lock();
    return instances.has(object->get_instance_id());
}


Error SchemeScript::_reload(bool keep_state) {
    // TODO: update instances dynamically
    return load();
}


Dictionary SchemeScript::_get_method_info(const StringName &method) const {
    HashMap<StringName, GDMethod>::ConstIterator method_info =
        class_definition.methods.find(method);
    if (method_info) {
        return Dictionary(method_info->value);
    }
    return Dictionary();
}


Variant SchemeScript::_get_property_default_value(const StringName &property) const {
    HashMap<StringName, uint64_t>::ConstIterator prop =
        class_definition.property_indices.find(property);
    if (prop) {
        return prop->value;
    }
    if (class_definition.has_base_script()) {
        return class_definition.base_script->get_property_default_value(property);
    }
    return Variant();
}


int32_t SchemeScript::_get_member_line(const StringName &member) const {
    // language->binder->
    return 0;
}


Variant SchemeScript::_get_rpc_config() const {
    Dictionary rpcs;

    for (const KeyValue<StringName, GDRpc> &pair : class_definition.rpcs)
        rpcs[pair.key] = pair.value;

    return rpcs;
}



// ResourceLoader //////////////////////////////////////////////////////////////

SchemeScriptResourceLoader::SchemeScriptResourceLoader() {}


SchemeScriptResourceLoader::~SchemeScriptResourceLoader() {}


Variant SchemeScriptResourceLoader::_load(const String &path, const String &original_path, bool use_sub_threads, int32_t cache_mode) const {
        // Ref<SchemeScript> script = resource;
	// ERR_FAIL_COND_V(script.is_null(), nullptr);

	// String source = script->get_source_code();

    Ref<SchemeScript> script;// = language->module_create_script();
    script.instantiate();

	Error err;
	Ref<FileAccess> file = FileAccess::open(path, FileAccess::READ);
	ERR_FAIL_COND_V_MSG(file.is_null(), FileAccess::get_open_error(), "Cannot open file '" + path + "'.");
	script->source_code = file->get_as_text();
	if (file->get_error() != OK && file->get_error() != ERR_FILE_EOF) {
		return ERR_CANT_CREATE;
	}
    // const SchemeScript scr = *script.ptr();

    script->load();
    // script->class_definition = language->binder.scheme_create_definition(scr);
    SchemeLanguage::get_singleton()->scripts.insert(path, script);

    return script;
}


PackedStringArray SchemeScriptResourceLoader::_get_recognized_extensions() const {
    // TODO: Add support for module files
    PackedStringArray extensions;
    extensions.append(SchemeLanguage::s_get_extension());
    return extensions;
}


bool SchemeScriptResourceLoader::_handles_type(const StringName& p_type) const {
    return (p_type == SchemeLanguage::s_get_type());
}

String SchemeScriptResourceLoader::_get_resource_type(const String& p_path) const {
    // String el = p_path.get_extension().to_lower();
    // if (el == SchemeLanguage::module_get_extension_constant()) {
    //     return SchemeLanguage::module_get_type_constant();
    // }
    // return "";
    return SchemeLanguage::s_get_type();
}

PackedStringArray SchemeScriptResourceLoader::_get_dependencies(const String& path, bool add_types) const {
    return PackedStringArray();
}

bool SchemeScriptResourceLoader::_recognize_path(const String &path, const StringName &type) const {
	return path.get_extension() == SchemeLanguage::s_get_extension();
}


String SchemeScriptResourceLoader::_get_resource_script_class(const String &path) const {
    return SchemeLanguage::s_get_type();
}


// int64_t SchemeScriptResourceLoader::_get_resource_uid(const String &path) const {
//     return ResourceLoader::get_resource_uid(path);
// }


// Error SchemeScriptResourceLoader::_rename_dependencies(const String &path, const Dictionary &renames) const {

// }


// bool SchemeScriptResourceLoader::_exists(const String &path) const {

// }


// PackedStringArray SchemeScriptResourceLoader::_get_classes_used(const String &path) const {

// }




// ResourceSaver ///////////////////////////////////////////////////////////////

SchemeScriptResourceSaver::SchemeScriptResourceSaver() {}


SchemeScriptResourceSaver::~SchemeScriptResourceSaver() {}


Error SchemeScriptResourceSaver::_save(const Ref<Resource> &resource, const String &path, uint32_t flags) {
    // Ref<FileAccess> out;
    // out.instantiate();
    // // will trigger https://github.com/godotengine/godot/issues/64455
    // out->open(path, FileAccess::ModeFlags::WRITE);
    // out->store_string("dummy");
    // out->close();TypedArray<Dictionary>
    // return OK;
    Ref<SchemeScript> script = resource;
	ERR_FAIL_COND_V(script.is_null(), ERR_INVALID_PARAMETER);

	String source = script->get_source_code();

	Error err;
	Ref<FileAccess> file = FileAccess::open(path, FileAccess::WRITE);
	ERR_FAIL_COND_V_MSG(file.is_null(), FileAccess::get_open_error(), "Cannot save file '" + path + "'.");
	file->store_string(source);
	if (file->get_error() != OK && file->get_error() != ERR_FILE_EOF) {
		return ERR_CANT_CREATE;
	}
    UtilityFunctions::print("Saved scheme script");

    // script->reload();
	// if (ScriptServer::is_reload_scripts_on_save_enabled()) {
	// 	script->reload();
	// }

	return OK;
}


bool SchemeScriptResourceSaver::_recognize(const Ref<Resource>& p_resource) const {
    // REVISIT: expanded for Natvis development
    const String resource_class = p_resource->get_class();
    const String our_class = SchemeScript::get_class_static();
    return (resource_class == our_class);
}


PackedStringArray SchemeScriptResourceSaver::_get_recognized_extensions(const Ref<Resource>& resource) const {
    PackedStringArray extensions;
    extensions.append(SchemeLanguage::s_get_extension());
    return extensions;
}
