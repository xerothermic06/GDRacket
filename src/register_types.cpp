// #include "register_types.h"

// #include "gdexample.h"

// #include <gdextension_interface.h>
// #include <godot_cpp/core/defs.hpp>
// #include <godot_cpp/core/class_db.hpp>
// #include <godot_cpp/godot.hpp>

// using namespace godot;

// void initialize_example_module(ModuleInitializationLevel p_level) {
//     if (p_level != MODULE_INITIALIZATION_LEVEL_SCENE) {
//         return;
//     }

//     ClassDB::register_class<GDExample>();
// }

// void uninitialize_example_module(ModuleInitializationLevel p_level) {
//     if (p_level != MODULE_INITIALIZATION_LEVEL_SCENE) {
//         return;
//     }
// }

// extern "C" {
// // Initialization.
// GDExtensionBool GDE_EXPORT example_library_init(GDExtensionInterfaceGetProcAddress p_get_proc_address, const GDExtensionClassLibraryPtr p_library, GDExtensionInitialization *r_initialization) {
//     godot::GDExtensionBinding::InitObject init_obj(p_get_proc_address, p_library, r_initialization);

//     init_obj.register_initializer(initialize_example_module);
//     init_obj.register_terminator(uninitialize_example_module);
//     init_obj.set_minimum_library_initialization_level(MODULE_INITIALIZATION_LEVEL_SCENE);

//     return init_obj.init();
// }
// }


#include "register_types.h"

#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/core/defs.hpp>
#include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/engine.hpp>
#include <godot_cpp/classes/resource_loader.hpp>
#include <godot_cpp/classes/resource_saver.hpp>

#include "scheme_language.h"
#include "scheme_script.h"
#include "binder/chibi_scheme_binder.h"

// #include "test_language.h"

#include <cstdio>

using namespace godot;

auto INIT_LEVEL = MODULE_INITIALIZATION_LEVEL_SCENE;

Ref<SchemeScriptResourceLoader> loader;
Ref<SchemeScriptResourceSaver> saver;
SchemeLanguage* language = nullptr;

// TestLanguage* testlang = nullptr;
ChibiSchemeBinder binder;

void initialize_scheme_module(ModuleInitializationLevel p_level) {
    if (p_level != INIT_LEVEL) {
        return;
    }
    // ClassDB::register_class<GDExample>();
    // ClassDB::register_class<TestLanguage>();
    // testlang = memnew(TestLanguage());
    // CRASH_COND_MSG(Engine::get_singleton()->register_script_language(testlang) != OK, "language register failed");

    ClassDB::register_class<SchemeLanguage>();
    ClassDB::register_class<SchemeScript>();
    ClassDB::register_class<SchemeScriptResourceLoader>();
    ClassDB::register_class<SchemeScriptResourceSaver>();

    godot::UtilityFunctions::print("Scheme initialized");

    language = memnew(SchemeLanguage());
    language->set_binder(&binder);
    // Engine::get_singleton()->register_script_language(language);
    CRASH_COND_MSG(Engine::get_singleton()->register_script_language(language) != OK, "scheme: language register failed");

    godot::UtilityFunctions::print_verbose("Scheme registered");

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

    // can't unregister this and can never deallocate because of thread exit handlers
// #if 0
// 	if (language != nullptr)
// 	{
// 		memdelete(language);
// 		language = nullptr;
// 	}
// #endif
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
