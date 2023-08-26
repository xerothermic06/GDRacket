
// #include <godot_cpp/classes/engine.hpp>
// #include <godot_cpp/classes/file_access.hpp>
#include <godot_cpp/classes/ref.hpp>
// #include <godot_cpp/godot.hpp>
#include <godot_cpp/variant/array.hpp>
#include <godot_cpp/variant/string.hpp>
// #include <godot_cpp/variant/string_name.hpp>

#include "util/scheme_util.h"

String SchemeUtil::resource_type_hint(const String &p_type) {
    // see core/object/object.h
    Array hint_values;
    hint_values.resize(3);
    hint_values[0] = Variant::OBJECT;
    hint_values[1] = PROPERTY_HINT_RESOURCE_TYPE;
    hint_values[2] = p_type;

    return String("{0}/{1}:{2}").format(hint_values);
}
