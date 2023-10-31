#include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/engine.hpp>

#include "util/racket_class_db.h"
#include "binder/racket_binder_util.h"

// Compensates for ClassDB methods not exposed to GDExtension. May be a better way of doing this

Object* RacketClassDB::class_db;

Object* RacketClassDB::get_class_db() {
    if (class_db == nullptr) {
        StringName classdb_name = "ClassDB";
        class_db = Engine::get_singleton()->get_singleton("ClassDB"); //(Object*)godot::internal::gdextension_interface_global_get_singleton(&classdb_name);
        _debug_logln("{0}", class_db);
    }

    return class_db;
}

bool RacketClassDB::class_exists(const StringName &p_class_name) {
    return get_class_db()->call("class_exists", p_class_name);
}


bool RacketClassDB::class_has_method(const StringName &p_class_name, const StringName &p_method, bool p_no_inheritance) {
    return get_class_db()->call("class_has_method", p_class_name, p_method, p_no_inheritance);
}


bool RacketClassDB::is_parent_class(const StringName &p_class_name, const StringName &p_inherits) {
    static String method = "is_parent_class";
    return get_class_db()->call(method, p_class_name, p_inherits);
}


StringName RacketClassDB::get_parent_class(const StringName &p_class_name) {
    static String method = "get_parent_class";
    return get_class_db()->call(method, p_class_name);
}


Array RacketClassDB::class_get_property_list(const StringName &p_class_name, bool p_no_inherited) {
    static String method = "class_get_property_list";
    return get_class_db()->call(method, p_class_name, p_no_inherited);
}


Array RacketClassDB::class_get_method_list(const StringName &p_class_name, bool p_no_inherited) {
    static String method = "class_get_method_list";
    return get_class_db()->call(method, p_class_name, p_no_inherited);
}


Object* RacketClassDB::instantiate(const StringName &p_class_name) {
    static String method = "instantiate";
    return get_class_db()->call(method, p_class_name);
}
