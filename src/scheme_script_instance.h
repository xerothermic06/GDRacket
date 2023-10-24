#pragma once

#include "scheme_binder.h"
#include "scheme_error.h"
#include "scheme_language.h"

#include <godot_cpp/variant/string_name.hpp>
#include <godot_cpp/classes/ref.hpp>
#include <godot_cpp/core/object.hpp>


class SchemeScript;
class SchemeBinder;


// ScriptInstance for Scheme objects. Nominally extends godot::ScriptInstance which is not exposed
// in GDExtension.
class SchemeScriptInstance {
    friend class SchemeBinder;

    static GDExtensionScriptInstanceInfo instance_info;

    Ref<SchemeScript> script;
    // Godot object this instance is attached to
    godot::Object* owner_object;
    // Scheme object for this instance
    void* scheme_object;

public:
    explicit SchemeScriptInstance(Ref<SchemeScript> script,
                                       godot::Object* owner_object);
    ~SchemeScriptInstance();

    static GDExtensionScriptInstancePtr create_instance(const SchemeScript* parent, godot::Object* host_object);

    void* get_scheme_object() {
        return scheme_object;
    }

    void set_scheme_object(void* p_ptr) {
        scheme_object = p_ptr;
    }

	virtual bool set(const StringName &p_name, const Variant &p_value);
	virtual bool get(const StringName &p_name, Variant *r_ret) const;
	virtual void get_property_list(List<PropertyInfo> *p_properties) const;

	virtual Variant callp(const StringName &p_method, const Variant **p_args, int p_argcount, SchemeCallError &r_error);

	virtual Ref<SchemeScript> get_script() const;

    // free_property_list_func
    // free_property_list_func
	virtual bool property_can_revert(const StringName &p_name) const { return false; };
	virtual bool property_get_revert(const StringName &p_name, Variant &r_ret) const { return false; };
    // property_can_revert_func
    // property_get_revert_func

	virtual Object *get_owner() const { return owner_object; }
    // void _get_property_state(GDExtensionScriptInstanceDataPtr p_instance, GDExtensionScriptInstancePropertyStateAdd p_add_func, void *p_userdata);

	virtual void get_method_list(List<MethodInfo> *p_list) const;

    // free_method_list_func

    virtual Variant::Type get_property_type(const StringName &p_name, bool *r_is_valid = nullptr) const;

	virtual bool has_method(const StringName &p_method) const;

	virtual void notification(int p_notification);
    // void notification(GDExtensionScriptInstanceDataPtr p_instance, int32_t p_what);

    virtual void to_string(GDExtensionScriptInstanceDataPtr p_instance, GDExtensionBool *r_is_valid, GDExtensionStringPtr r_out) {
        String x("scheme");
        r_out = &x;
    }

    // refcount_incremented_func
    // refcount_decremented_func
    virtual bool is_placeholder() const { return false; };
    // set_fallback_func
    // get_fallback_func

	virtual ScriptLanguage *get_language();

    // free_func
};


class SchemeScriptInstanceGlue {
    static GDExtensionScriptInstanceInfo instance_info;
public:
    static GDExtensionPropertyInfo _convert_prop_info(PropertyInfo &p_info);
    static GDExtensionMethodInfo _convert_method_info(MethodInfo info);
    static GDExtensionCallError _convert_call_err(SchemeCallError p_err);
    static GDExtensionObjectPtr s_get_script(GDExtensionScriptInstanceDataPtr void_this);
    static void s_free(GDExtensionScriptInstanceDataPtr void_this);
    static GDExtensionBool s_has_method(GDExtensionScriptInstanceDataPtr p_instance, GDExtensionConstStringNamePtr p_name);
    static void s_call(
        GDExtensionScriptInstanceDataPtr p_instance,
        GDExtensionConstStringNamePtr p_method,
        const GDExtensionConstVariantPtr *p_args,
        GDExtensionInt p_argument_count,
        GDExtensionVariantPtr r_return,
        GDExtensionCallError *r_error
    );
    static GDExtensionBool s_set(
        GDExtensionScriptInstanceDataPtr p_instance,
        GDExtensionConstStringNamePtr p_name,
        GDExtensionConstVariantPtr p_value);
    static GDExtensionBool s_get(
        GDExtensionScriptInstanceDataPtr p_instance,
        GDExtensionConstStringNamePtr p_name,
        GDExtensionVariantPtr r_ret);
    static const GDExtensionPropertyInfo* s_get_property_list(
        GDExtensionScriptInstanceDataPtr p_instance,
        uint32_t *r_count);
    static void s_free_property_list(GDExtensionScriptInstanceDataPtr p_instance, const GDExtensionPropertyInfo *p_list);
    static GDExtensionVariantType s_get_property_type(
        GDExtensionScriptInstanceDataPtr p_instance,
        GDExtensionConstStringNamePtr p_name,
        GDExtensionBool *r_is_valid);
    static void s_notification(GDExtensionScriptInstanceDataPtr p_instance, int32_t p_what);
    static GDExtensionScriptLanguagePtr s_get_language(GDExtensionScriptInstanceDataPtr p_instance);
    static GDExtensionObjectPtr s_get_owner(GDExtensionScriptInstanceDataPtr p_instance);
    static const GDExtensionMethodInfo* s_get_method_list(GDExtensionScriptInstanceDataPtr p_instance, uint32_t *r_count);
    static void s_free_method_list(GDExtensionScriptInstanceDataPtr p_instance, const GDExtensionMethodInfo *p_list);

};

