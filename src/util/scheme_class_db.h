#pragma once

#include <godot_cpp/variant/string.hpp>
#include <godot_cpp/variant/string_name.hpp>
#include <godot_cpp/variant/variant.hpp>

using namespace godot;

class SchemeClassDB {
    static Object* class_db;
    static Object* get_class_db();

public:
    static bool class_exists(const StringName &p_class_name);
    static bool class_has_method(const StringName &p_class_name, const StringName &p_method, bool p_no_inheritance = false);
    static bool is_parent_class(const StringName &p_class_name, const StringName &p_inherits);
    static StringName get_parent_class(const StringName &p_class_name);
};

