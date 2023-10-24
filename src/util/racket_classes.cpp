
#include <gdextension_interface.h>
// #include <lua.h>
// #include <lualib.h>
#include <string.h>
#include <godot_cpp/classes/file_access.hpp>
#include <godot_cpp/classes/global_constants.hpp>
#include <godot_cpp/classes/ref.hpp>
#include <godot_cpp/core/memory.hpp>
#include <godot_cpp/core/object.hpp>
#include <godot_cpp/variant/builtin_types.hpp>
#include <godot_cpp/variant/char_string.hpp>
#include <godot_cpp/variant/variant.hpp>


#include "racket_classes.h"

using namespace godot;

// Record types for tracking exposed script metadata

// GDProperty //////////////////////////////////////////////////////////////////
GDProperty::operator Dictionary() const {
    Dictionary dict;
    dict["type"] = type;
    dict["name"] = name;
    dict["class_name"] = class_name;
    return dict;
}


GDProperty::operator Variant() const {
    return this->operator Dictionary();
}


GDProperty::operator PropertyInfo() const {
    return PropertyInfo(
        this->type,
        this->name,
        PropertyHint::PROPERTY_HINT_NONE, //this->hint,
        "", //this->hint_string,
        PropertyUsageFlags::PROPERTY_USAGE_NONE, //this->usage,
        this->class_name);
}


// GDClassProperty /////////////////////////////////////////////////////////////
GDClassProperty::operator PropertyInfo() const {
    PropertyInfo pinfo = this->property.operator PropertyInfo();

    pinfo.hint = this->hint;
    pinfo.hint_string = this->hint_string;
    pinfo.usage = this->usage;

    return pinfo;
}


GDClassProperty::operator Dictionary() const {
    Dictionary dict = this->property.operator Dictionary();

    dict["usage"] = usage;
    dict["hint"] = hint;
    dict["hint_string"] = hint_string;

    return dict;
}


// GDMethod ////////////////////////////////////////////////////////////////////
GDMethod::operator Dictionary() const {
    Dictionary dict;

    dict["name"] = name;
    dict["return"] = return_val;
    dict["flags"] = flags;

    Array args;
    for (const GDProperty &arg : arguments)
        args.push_back(arg);

    dict["args"] = args;

    Array default_args;
    for (const Variant &default_arg : default_arguments)
        default_args.push_back(default_arg);

    dict["default_args"] = default_args;

    return dict;
}


GDMethod::operator Variant() const {
    return operator Dictionary();
}


GDMethod::operator MethodInfo() const {
    return MethodInfo((PropertyInfo)this->return_val, this->name);
}


// GDRpc ///////////////////////////////////////////////////////////////////////
GDRpc::operator Dictionary() const {
    Dictionary dict;

    dict["rpc_mode"] = rpc_mode;
    dict["transfer_mode"] = transfer_mode;
    dict["call_local"] = call_local;
    dict["channel"] = channel;

    return dict;
}

GDRpc::operator Variant() const {
    return operator Dictionary();
}


bool GDClassDefinition::has_prop(const StringName &p_name) const {
    return property_indices.has(p_name);
}


const GDClassProperty* GDClassDefinition::get_prop(const StringName &p_name) const {
    return &(properties[property_indices.get(p_name)]);
}


int GDClassDefinition::register_prop(const StringName &p_name, const GDClassProperty &p_prop) {
    HashMap<StringName, uint64_t>::ConstIterator E = property_indices.find(p_name);

    if (E) {
        properties.set(E->value, p_prop);
        return E->value;
    } else {
        int index = properties.size();
        property_indices[p_name] = index;
        properties.push_back(p_prop);

        return index;
    }
}


bool GDClassDefinition::has_method(const StringName &p_name) const {
    return methods.has(p_name);
}


void GDClassDefinition::register_method(const StringName &p_name, const GDMethod &p_method) {
    methods.insert(p_name, p_method);
}


const GDMethod* GDClassDefinition::get_method(const StringName &p_name) const {
    return &methods.get(p_name);
}


bool GDClassDefinition::has_base_script() const {
    return base_script.is_valid();
}
