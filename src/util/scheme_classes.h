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

// #include "scheme_script.h"
#include "util/scheme_class_db.h"
#include "util/scheme_util.h"

// Circular dep
class SchemeScript;

using namespace godot;


struct GDProperty {
    GDExtensionVariantType type = GDEXTENSION_VARIANT_TYPE_NIL;
    BitField<PropertyUsageFlags> usage = PROPERTY_USAGE_DEFAULT;

    StringName name;
    StringName class_name;

    PropertyHint hint = PROPERTY_HINT_NONE;
    String hint_string;

    operator Dictionary() const;
    operator Variant() const;

    void set_variant_type() {
        type = GDEXTENSION_VARIANT_TYPE_NIL;
        usage = PROPERTY_USAGE_DEFAULT | PROPERTY_USAGE_NIL_IS_VARIANT;
    }

    void set_object_type(const String &p_type) {
        type = GDEXTENSION_VARIANT_TYPE_OBJECT;

        if (SchemeClassDB::is_parent_class(p_type, "Resource")) {
            hint = PROPERTY_HINT_RESOURCE_TYPE;
            hint_string = p_type;
        } else {
            class_name = p_type;
        }
    }

    void set_typed_array_type(const GDProperty &p_type) {
        type = GDEXTENSION_VARIANT_TYPE_ARRAY;
        hint = PROPERTY_HINT_ARRAY_TYPE;

        if (p_type.type == GDEXTENSION_VARIANT_TYPE_OBJECT) {
            if (p_type.hint == PROPERTY_HINT_RESOURCE_TYPE) {
                hint_string = SchemeUtil::resource_type_hint(p_type.hint_string);
            } else {
                hint_string = p_type.class_name;
            }
        } else {
            hint_string = Variant::get_type_name(Variant::Type(p_type.type));
        }
    }
};

struct GDClassProperty {
    GDProperty property;

    StringName getter;
    StringName setter;

    Variant default_value;
};

struct GDMethod {
    String name;
    GDProperty return_val;
    BitField<MethodFlags> flags = METHOD_FLAGS_DEFAULT;
    Vector<GDProperty> arguments;
    Vector<Variant> default_arguments;

    operator Dictionary() const;
    operator Variant() const;
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

// class LuauScript;

struct GDClassDefinition {
    String name;
    String extends = "RefCounted";
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

    int set_prop(const String &p_name, const GDClassProperty &p_prop);
    void set_method(const String &p_name, const GDMethod &p_method);
    bool has_method(const String &p_name) const;
    bool has_base_script() const;
};

