#ifndef RACKET_UTIL_H
#define RACKET_UTIL_H

#include <godot_cpp/variant/string.hpp>

using namespace godot;

class RacketUtil {
public:

    static String resource_type_hint(const String &p_type);
    static const char* string_to_charptr(const String s);
    static void log(const String msg);
    static void logln(const String msg);
};

#endif // RACKET_UTIL_H
