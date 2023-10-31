#include "godot_cpp/classes/file_access.hpp"

#include "racket_language.h"
#include "racket_script.h"


#define GD_RKT_EXT ".gd.rkt"
#define RACKET_MODULE "RacketModule"

RacketScript::RacketScript() {
    language = RacketLanguage::get_singleton();
}


RacketScript::~RacketScript() {
}


Error RacketScript::load() {
    GDClassDefinition def;
    Error err = language->binder->create_definition(*this, def);
    is_valid = err == Error::OK;
    return err;
    // return Error::OK;
}


bool RacketScript::_can_instantiate() const {
    return is_valid;
}


bool RacketScript::_has_source_code() const {
    return !source_code.is_empty();
}


bool RacketScript::_has_method(const StringName& method) const {
    return class_definition.methods.has(method);
}


bool RacketScript::_has_static_method(const StringName& method) const {
    return false; //class_definition.methods.has(method);
}


bool RacketScript::_is_tool() const {
    return false;
}


bool RacketScript::_is_valid() const {
    return true; //is_valid;
}


ScriptLanguage* RacketScript::_get_language() const {
    return language;
}


bool RacketScript::_has_script_signal(const StringName& signal) const {
    // TODO: check for equivalent hyphentated sexpr name
    return class_definition.signals.has(signal);
}


TypedArray<Dictionary> RacketScript::_get_script_signal_list() const {
    auto signals = Array();
    for (const KeyValue<StringName, GDMethod> kv : class_definition.signals) {
        signals.push_back(Dictionary(kv.value));
    }
    return signals;
}


TypedArray<Dictionary> RacketScript::_get_script_method_list() const {
    auto methods = Array();
    for (const KeyValue<StringName, GDMethod> kv : class_definition.methods) {
        methods.push_back(Dictionary(kv.value));
    }
    return methods;
}


TypedArray<Dictionary> RacketScript::_get_script_property_list() const {
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


Dictionary RacketScript::_get_constants() const {
    return Dictionary();
}


TypedArray<StringName> RacketScript::_get_members() const {
    TypedArray<StringName> members;

    for (const GDClassProperty &prop : class_definition.properties)
        members.push_back(prop.property.name);

    return members;
}


bool RacketScript::_is_placeholder_fallback_enabled() const {
    return false;
}


Ref<Script> RacketScript::_get_base_script() const {
    return Ref<Script>();
}


StringName RacketScript::_get_instance_base_type() const {
    StringName extends = StringName(class_definition.extends);

    if (extends != StringName() && RacketClassDB::class_exists(extends)) {
        return extends;
    }

    auto base_script = class_definition.base_script;
    if (class_definition.has_base_script()) {
        return base_script->get_instance_base_type();
    }
    return StringName();
}


String RacketScript::_get_source_code() const {
    return source_code;
}


void RacketScript::_set_source_code(const String& code) {
    source_code = code;
}


void RacketScript::_update_exports() {
    // TODO: Figure out what this is supposed to do
}


void* RacketScript::_instance_create(Object* for_object) const {
    return RacketScriptInstance::create_instance(this, for_object);
}

void* RacketScript::_placeholder_instance_create(Object *for_object) const {
    return nullptr;
}


TypedArray<Dictionary> RacketScript::_get_documentation() const {
    return Array();
}


bool RacketScript::_has_property_default_value(const StringName&) const {
    return false;
}


bool RacketScript::_editor_can_reload_from_file() {
    return true;
}


StringName RacketScript::_get_global_name() const {
    return get_name();
}


bool RacketScript::_inherits_script(const Ref<Script> &p_script) const {
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


bool RacketScript::_instance_has(Object *object) const {
    RacketLanguage::get_singleton()->get_instance_lock();
    return instances.has(object->get_instance_id());
}


Error RacketScript::_reload(bool keep_state) {
    // TODO: update instances dynamically
    return load();
}


Dictionary RacketScript::_get_method_info(const StringName &method) const {
    HashMap<StringName, GDMethod>::ConstIterator method_info =
        class_definition.methods.find(method);
    if (method_info) {
        return Dictionary(method_info->value);
    }
    return Dictionary();
}


Variant RacketScript::_get_property_default_value(const StringName &property) const {
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


int32_t RacketScript::_get_member_line(const StringName &member) const {
    return 0;
}


Variant RacketScript::_get_rpc_config() const {
    Dictionary rpcs;

    for (const KeyValue<StringName, GDRpc> &pair : class_definition.rpcs)
        rpcs[pair.key] = pair.value;

    return rpcs;
}


// ResourceLoader //////////////////////////////////////////////////////////////

RacketScriptResourceLoader::RacketScriptResourceLoader() {}


RacketScriptResourceLoader::~RacketScriptResourceLoader() {}


Variant RacketScriptResourceLoader::_load_module(const String &path) const {
    Ref<RacketModule> modul;
    modul.instantiate();
    Error err;
    Ref<FileAccess> file = FileAccess::open(path, FileAccess::READ);
    ERR_FAIL_COND_V_MSG(file.is_null(), FileAccess::get_open_error(), "Cannot open file '" + path + "'.");
    modul->source_code = file->get_as_text();
    if (file->get_error() != OK && file->get_error() != ERR_FILE_EOF) {
        return ERR_CANT_CREATE;
    }
    modul->set_path(path);
    return modul;
}

Variant RacketScriptResourceLoader::_load_script(const String &path) const {
    Ref<RacketScript> script;
    script.instantiate();

    Error err;
    Ref<FileAccess> file = FileAccess::open(path, FileAccess::READ);
    ERR_FAIL_COND_V_MSG(file.is_null(), FileAccess::get_open_error(), "Cannot open file '" + path + "'.");
    script->source_code = file->get_as_text();
    if (file->get_error() != OK && file->get_error() != ERR_FILE_EOF) {
        return ERR_CANT_CREATE;
    }
    script->set_path(path);
    script->load();
    RacketLanguage::get_singleton()->scripts.insert(path, script);

    return script;
}

Variant RacketScriptResourceLoader::_load(const String &path, const String &original_path, bool use_sub_threads, int32_t cache_mode) const {
    if (path.ends_with(GD_RKT_EXT)) {
        return _load_script(path);
    }
    return _load_module(path);

}


PackedStringArray RacketScriptResourceLoader::_get_recognized_extensions() const {
    // TODO: Add support for module files
    PackedStringArray extensions;
    extensions.append(RacketLanguage::s_get_extension());
    return extensions;
}


bool RacketScriptResourceLoader::_handles_type(const StringName& p_type) const {
    return (p_type == RacketLanguage::s_get_type() || p_type == StringName(RACKET_MODULE));
}

String RacketScriptResourceLoader::_get_resource_type(const String& p_path) const {
    if (p_path.ends_with(GD_RKT_EXT)) {
        return RacketLanguage::s_get_type();
    }
    return RACKET_MODULE;
}

PackedStringArray RacketScriptResourceLoader::_get_dependencies(const String& path, bool add_types) const {
    // TODO: link script deps to modules?
    return PackedStringArray();
}

bool RacketScriptResourceLoader::_recognize_path(const String &path, const StringName &type) const {
	return path.get_extension() == RacketLanguage::s_get_extension();
}


String RacketScriptResourceLoader::_get_resource_script_class(const String &path) const {
    if (path.ends_with(GD_RKT_EXT)) {
        return RacketLanguage::s_get_type();
    }
    return RACKET_MODULE;
}


// ResourceSaver ///////////////////////////////////////////////////////////////

RacketScriptResourceSaver::RacketScriptResourceSaver() {}


RacketScriptResourceSaver::~RacketScriptResourceSaver() {}


Error RacketScriptResourceSaver::_save(const Ref<Resource> &resource, const String &path, uint32_t flags) {
    String source;
    if (path.ends_with(GD_RKT_EXT)) {
        Ref<RacketScript> script = resource;
        ERR_FAIL_COND_V(script.is_null(), ERR_INVALID_PARAMETER);
        source = script->get_source_code();
    } else {
        Ref<RacketModule> modul = resource;
        source = modul->get_source_code();
    }


	Error err;
	Ref<FileAccess> file = FileAccess::open(path, FileAccess::WRITE);
	ERR_FAIL_COND_V_MSG(file.is_null(), FileAccess::get_open_error(), "Cannot save file '" + path + "'.");
	file->store_string(source);
	if (file->get_error() != OK && file->get_error() != ERR_FILE_EOF) {
		return ERR_CANT_CREATE;
	}

	return OK;
}


bool RacketScriptResourceSaver::_recognize(const Ref<Resource>& p_resource) const {
    const String resource_class = p_resource->get_class();
    const String our_class = RacketScript::get_class_static();
    return (resource_class == our_class);
}


PackedStringArray RacketScriptResourceSaver::_get_recognized_extensions(const Ref<Resource>& resource) const {
    PackedStringArray extensions;
    extensions.append(RacketLanguage::s_get_extension());
    return extensions;
}
