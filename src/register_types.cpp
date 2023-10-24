#include "register_types.h"

#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/core/defs.hpp>
#include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/engine.hpp>
#include <godot_cpp/classes/resource_loader.hpp>
#include <godot_cpp/classes/resource_saver.hpp>

#include "scheme_language.h"
#include "scheme_script.h"

#include <cstdio>

using namespace godot;

auto INIT_LEVEL = MODULE_INITIALIZATION_LEVEL_SCENE;

Ref<SchemeScriptResourceLoader> loader;
Ref<SchemeScriptResourceSaver> saver;

SchemeLanguage* language = nullptr;

void initialize_scheme_module(ModuleInitializationLevel p_level) {
    if (p_level != INIT_LEVEL) {
        return;
    }

    ClassDB::register_class<SchemeLanguage>();
    ClassDB::register_class<SchemeScript>();
    ClassDB::register_class<SchemeScriptResourceLoader>();
    ClassDB::register_class<SchemeScriptResourceSaver>();

    language = memnew(SchemeLanguage());
    CRASH_COND_MSG(Engine::get_singleton()->register_script_language(language) != OK, "scheme: language register failed");

    loader.instantiate();
    loader->language = language;
    ResourceLoader::get_singleton()->add_resource_format_loader(loader);

    saver.instantiate();
    ResourceSaver::get_singleton()->add_resource_format_saver(saver);

}

void uninitialize_scheme_module(ModuleInitializationLevel p_level) {
    if (p_level != INIT_LEVEL) {
        return;
    }
    ResourceLoader::get_singleton()->remove_resource_format_loader(loader);
    ResourceSaver::get_singleton()->remove_resource_format_saver(saver);

    loader.unref();
    saver.unref();
    delete language;

}

extern "C" {

// Initialization.

GDExtensionBool GDE_EXPORT gdscheme_init(GDExtensionInterfaceGetProcAddress p_get_proc_address, const GDExtensionClassLibraryPtr p_library, GDExtensionInitialization *r_initialization) {
    godot::GDExtensionBinding::InitObject init_obj(p_get_proc_address, p_library, r_initialization);

    init_obj.register_initializer(initialize_scheme_module);
    init_obj.register_terminator(uninitialize_scheme_module);
    init_obj.set_minimum_library_initialization_level(INIT_LEVEL);

    return init_obj.init();
}

}
