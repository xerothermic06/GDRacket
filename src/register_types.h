// #ifndef GDEXAMPLE_REGISTER_TYPES_H
// #define GDEXAMPLE_REGISTER_TYPES_H

// void initialize_example_module();
// void uninitialize_example_module();

// #endif // GDEXAMPLE_REGISTER_TYPES_H

#pragma once

#include "scheme_lang.h"

#include <godot_cpp/core/class_db.hpp>

void initialize_scheme_module(godot::ModuleInitializationLevel p_level);
void uninitialize_scheme_module(godot::ModuleInitializationLevel p_level);
// GDExtensionBool GDE_EXPORT gdscheme_init(GDExtensionInterfaceGetProcAddress p_get_proc_address, const GDExtensionClassLibraryPtr p_library, GDExtensionInitialization *r_initialization);
