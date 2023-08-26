
// #include <godot_cpp/classes/engine.hpp>
// #include <godot_cpp/classes/file_access.hpp>
// #include <godot_cpp/classes/ref.hpp>
// #include <godot_cpp/godot.hpp>
// #include <godot_cpp/variant/array.hpp>
// #include <godot_cpp/variant/string.hpp>
// #include <godot_cpp/variant/string_name.hpp>
#include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/engine.hpp>

#include "scheme_class_db.h"

Object* SchemeClassDB::class_db;

Object* SchemeClassDB::get_class_db() {
    if (class_db == nullptr) {
        StringName classdb_name = "ClassDB";
        class_db = (Object*)godot::internal::gdextension_interface_global_get_singleton(&classdb_name);
    }
    return class_db;
}

bool SchemeClassDB::class_exists(const StringName &p_class_name) {
    return get_class_db()->call("class_exists", p_class_name);
}

bool SchemeClassDB::class_has_method(const StringName &p_class_name, const StringName &p_method, bool p_no_inheritance) {
    return get_class_db()->call("class_has_method", p_class_name, p_method, p_no_inheritance);
}

bool SchemeClassDB::is_parent_class(const StringName &p_class_name, const StringName &p_inherits) {
    return get_class_db()->call("is_parent_class", p_class_name, p_inherits);
}

StringName SchemeClassDB::get_parent_class(const StringName &p_class_name) {
    return get_class_db()->call("get_parent_class", p_class_name);
}
