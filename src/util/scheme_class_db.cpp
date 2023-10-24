#include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/engine.hpp>

#include "scheme_class_db.h"
#include "../binder/racket_binder_util.h"
// Compensates for ClassDB methods not exposed to GDExtension. May be a better way of doing this

Object* SchemeClassDB::class_db;

Object* SchemeClassDB::get_class_db() {
    if (class_db == nullptr) {
        StringName classdb_name = "ClassDB";
        class_db = Engine::get_singleton()->get_singleton("ClassDB"); //(Object*)godot::internal::gdextension_interface_global_get_singleton(&classdb_name);
        _debug_logln("{0}", class_db);
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


Array SchemeClassDB::class_get_property_list(const StringName &p_class_name) {
    return get_class_db()->call("class_get_property_list", p_class_name);
}


Array SchemeClassDB::class_get_method_list(const StringName &p_class_name) {
    return get_class_db()->call("class_get_method_list", p_class_name);
}


Object* SchemeClassDB::instantiate(const StringName &p_class_name) {
    return get_class_db()->call("instantiate", p_class_name);
}
