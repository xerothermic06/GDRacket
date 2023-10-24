#ifndef REGISTER_TYPES_H
#define REGISTER_TYPES_H

// This may not be necessary
// #pragma warning(disable : 26812)

#include <godot_cpp/core/class_db.hpp>

void initialize_racket_module(godot::ModuleInitializationLevel p_level);
void uninitialize_racket_module(godot::ModuleInitializationLevel p_level);

#endif // REGISTER_TYPES_H
