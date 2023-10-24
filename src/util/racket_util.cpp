#include "util/racket_util.h"

#include <iostream>

#include <godot_cpp/classes/ref.hpp>
#include <godot_cpp/variant/array.hpp>
#include <godot_cpp/variant/string.hpp>


String RacketUtil::resource_type_hint(const String &p_type) {
    Array hint_values;
    hint_values.resize(3);
    hint_values[0] = Variant::OBJECT;
    hint_values[1] = PROPERTY_HINT_RESOURCE_TYPE;
    hint_values[2] = p_type;

    return String("{0}/{1}:{2}").format(hint_values);
}


const char* RacketUtil::string_to_charptr(const String s) {
    CharString st = s.utf8();
    return st.get_data();
}


void RacketUtil::log(const String msg) {
    std::cout << msg.utf8().get_data();
}


void RacketUtil::logln(const String msg) {
    std::cout << msg.utf8().get_data() << std::endl;
}
