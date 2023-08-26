#pragma once
// #include "gdextension_interface.h"
// #include "wrapped_no_binding.h"
#include <godot_cpp/variant/string.hpp>
// #include <godot_cpp/variant/string_name.hpp>
// #include <godot_cpp/variant/variant.hpp>

using namespace godot;

class SchemeUtil {
public:
    static String resource_type_hint(const String &p_type);
};
