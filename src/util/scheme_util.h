#pragma once
#include <godot_cpp/variant/string.hpp>

using namespace godot;

class SchemeUtil {
public:

    static String resource_type_hint(const String &p_type);
    static const char* string_to_charptr(const String s);
    static void log(const String msg);
    static void logln(const String msg);
};
