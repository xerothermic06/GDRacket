#pragma once

#include <gdextension_interface.h>

#include <godot_cpp/classes/global_constants.hpp>
#include <godot_cpp/classes/multiplayer_api.hpp>
#include <godot_cpp/classes/multiplayer_peer.hpp>
#include <godot_cpp/classes/script.hpp>
#include <godot_cpp/core/type_info.hpp>
#include <godot_cpp/templates/hash_map.hpp>
#include <godot_cpp/templates/vector.hpp>
#include <godot_cpp/variant/callable.hpp>
#include <godot_cpp/variant/dictionary.hpp>
#include <godot_cpp/variant/signal.hpp>
#include <godot_cpp/variant/string.hpp>
#include <godot_cpp/variant/string_name.hpp>
#include <godot_cpp/variant/variant.hpp>

#include "util/scheme_class_db.h"
#include "util/scheme_util.h"

// Circular dep
class SchemeScript;

using namespace godot;


// Script class registration metadata structs. Some of these are duplicates of
// existing structs (PropertyInfo, MethodInfo, etc.) But are different structs
// to acommodate implementation-specific metadata if needed.


struct GDProperty {
    GDExtensionVariantType type = GDEXTENSION_VARIANT_TYPE_NIL;
    // BitField<PropertyUsageFlags> usage = PROPERTY_USAGE_DEFAULT;

    StringName name;
    StringName class_name;

    operator Dictionary() const;
    operator Variant() const;
    operator PropertyInfo() const;

    // void set_variant_type() {
    //     type = GDEXTENSION_VARIANT_TYPE_NIL;
    //     usage = PROPERTY_USAGE_DEFAULT | PROPERTY_USAGE_NIL_IS_VARIANT;
    // }

    // void set_object_type(const String &p_type) {
    //     type = GDEXTENSION_VARIANT_TYPE_OBJECT;

    //     if (SchemeClassDB::is_parent_class(p_type, "Resource")) {
    //         hint = PROPERTY_HINT_RESOURCE_TYPE;
    //         hint_string = p_type;
    //     } else {
    //         class_name = p_type;
    //     }
    // }

    // void set_typed_array_type(const GDProperty &p_type) {
    //     type = GDEXTENSION_VARIANT_TYPE_ARRAY;
    //     hint = PROPERTY_HINT_ARRAY_TYPE;

    //     if (p_type.type == GDEXTENSION_VARIANT_TYPE_OBJECT) {
    //         if (p_type.hint == PROPERTY_HINT_RESOURCE_TYPE) {
    //             hint_string = SchemeUtil::resource_type_hint(p_type.hint_string);
    //         } else {
    //             hint_string = p_type.class_name;
    //         }
    //     } else {
    //         hint_string = Variant::get_type_name(Variant::Type(p_type.type));
    //     }
    // }
};

struct GDClassProperty {
    GDProperty property;

    Variant default_value;

    String hint_string;

    // Scheme symbol name associated with the property. Scheme allows far more
    // leeway in identifier names so this must be tracked to index into
    // instances.
    StringName symbol;
    StringName getter;
    StringName setter;

    PropertyHint hint        = PROPERTY_HINT_NONE;
    PropertyUsageFlags usage = PROPERTY_USAGE_NONE;

    operator Dictionary() const;
    operator PropertyInfo() const;
};

struct GDMethod {
    StringName name;
    StringName symbol;
    GDProperty return_val;
    BitField<MethodFlags> flags = METHOD_FLAGS_DEFAULT;
    Vector<GDProperty> arguments;
    Vector<Variant> default_arguments;

    GDMethod() {};

    GDMethod(StringName p_name, StringName p_symbol, GDProperty p_return_val):
        name(p_name), symbol(p_symbol), return_val(p_return_val) {};

    operator Dictionary() const;
    operator Variant() const;
    operator MethodInfo() const;
};

// ! Reference: modules/multiplayer/scene_rpc_interface.cpp _parse_rpc_config
struct GDRpc {
    String name;
    MultiplayerAPI::RPCMode rpc_mode = MultiplayerAPI::RPC_MODE_AUTHORITY;
    MultiplayerPeer::TransferMode transfer_mode = MultiplayerPeer::TRANSFER_MODE_RELIABLE;
    bool call_local = false;
    int channel = 0;

    operator Dictionary() const;
    operator Variant() const;
};


struct GDClassDefinition {
    String name;
    String extends = "Object";
    Ref<Script> base_script = nullptr;

    String icon_path;

    // ThreadPermissions permissions = PERMISSION_BASE;

    bool is_tool = false;

    HashMap<StringName, GDMethod> methods;
    HashMap<StringName, uint64_t> property_indices;
    Vector<GDClassProperty> properties;
    HashMap<StringName, GDMethod> signals;
    HashMap<StringName, GDRpc> rpcs;
    HashMap<StringName, int> constants;

    bool has_prop(const StringName &p_name) const;
    const GDClassProperty* GDClassDefinition::get_prop(const StringName &p_name) const;
    int register_prop(const StringName &p_name, const GDClassProperty &p_prop);

    bool has_method(const StringName &p_name) const;
    void register_method(const StringName &p_name, const GDMethod &p_method);
    const GDMethod* get_method(const StringName &p_name) const;

    bool has_base_script() const;
};
