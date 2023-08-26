#pragma once

#include "scheme_binder.h"
#include "scheme_error.h"
#include "scheme_language.h"

#include <godot_cpp/variant/string_name.hpp>
#include <godot_cpp/classes/ref.hpp>
#include <godot_cpp/core/object.hpp>

// #include <godot-cpp/gdextension/gdextension_interface.h>

// class SchemeLanguage;
class SchemeScript;

// ReSharper disable CppClangTidyMiscMisplacedConst
// ReSharper disable CppMemberFunctionMayBeConst

// ScriptInstance for Scheme objects. Nominally extends godot::ScriptInstance which is not exposed
// in GDExtension for some reason.
class SchemeScriptInstance {
    Ref<SchemeScript> script;
    // Godot object this instance is attached to
    godot::Object* owner_object;
    // Scheme object
    // SchemeObjectWrapper scheme_object;
    // std::unique_ptr<GDExtensionObjectPtr> backingObject;
    // plugin-to-scheme interface
    SchemeBinder *binder;

public:
    explicit SchemeScriptInstance(Ref<SchemeScript> script,
                                       godot::Object* owner_object);
    ~SchemeScriptInstance();

    static GDExtensionScriptInstancePtr create_instance(const SchemeScript* parent, godot::Object* host_object);

	virtual bool set(const StringName &p_name, const Variant &p_value);
	virtual bool get(const StringName &p_name, Variant &r_ret) const;
	virtual void get_property_list(List<PropertyInfo> *p_properties) const;

	virtual Variant callp(const StringName &p_method, const Variant **p_args, int p_argcount, SchemeCallError &r_error);

	virtual Ref<Script> get_script() const;

    // free_property_list_func
    // free_property_list_func
	virtual bool property_can_revert(const StringName &p_name) const { return false; };
	virtual bool property_get_revert(const StringName &p_name, Variant &r_ret) const { return false; };
    // property_can_revert_func
    // property_get_revert_func

	virtual Object *get_owner() { return owner_object; }
    // void _get_property_state(GDExtensionScriptInstanceDataPtr p_instance, GDExtensionScriptInstancePropertyStateAdd p_add_func, void *p_userdata);

	virtual void get_method_list(List<MethodInfo> *p_list) const;

    // free_method_list_func

    virtual Variant::Type get_property_type(const StringName &p_name, bool *r_is_valid = nullptr) const;

	virtual bool has_method(const StringName &p_method) const;

	virtual void notification(int p_notification) {};
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
    static GDExtensionScriptInstanceInfo get_instance_info() {
        if (instance_info.set_func == nullptr) {
            instance_info.set_func = s_set;
            instance_info.get_func = s_get;

            instance_info.get_property_list_func = s_get_property_list;
            instance_info.free_property_list_func = s_free_property_list;

            // property_can_revert_func
            // property_get_revert_func

            instance_info.get_owner_func = s_get_owner;
            // get_property_state_func

            instance_info.get_method_list_func = s_get_method_list;
            instance_info.free_method_list_func = s_free_method_list;

            instance_info.get_property_type_func = s_get_property_type;
            instance_info.has_method_func = s_has_method;
            instance_info.call_func = s_call;
            instance_info.notification_func = s_notification;

            instance_info.get_language_func = s_get_language;
            // to_string_func
            // refcount_incremented_func
            // refcount_decremented_func
            instance_info.get_script_func = s_get_script;
            // is_placeholder_func
            // set_fallback_func
            // get_fallback_func
            // get_language_func
            instance_info.free_func = s_free;
        }
        return instance_info;
    }

private:
    // dumb copy-paste, probably don't need?
    // class Bindings {
    // public:
    //     Bindings() {
    //         instance_info.set_func = s_set;
    //         instance_info.get_func = s_get;

    //         instance_info.get_property_list_func = s_get_property_list;
    //         instance_info.free_property_list_func = s_free_property_list;

    //         // property_can_revert_func
    //         // property_get_revert_func

    //         instance_info.get_owner_func = s_get_owner;
    //         // get_property_state_func

    //         instance_info.get_method_list_func = s_get_method_list;
    //         instance_info.free_method_list_func = s_free_method_list;

    //         instance_info.get_property_type_func = s_get_property_type;
    //         instance_info.has_method_func = s_has_method;
    //         instance_info.call_func = s_call;
    //         instance_info.notification_func = s_notification;

    //         instance_info.get_language_func = s_get_language;
    //         // to_string_func
    //         // refcount_incremented_func
    //         // refcount_decremented_func
    //         instance_info.get_script_func = s_get_script;
    //         // is_placeholder_func
    //         // set_fallback_func
    //         // get_fallback_func
    //         // get_language_func
    //         instance_info.free_func = s_free;
    //     }
    // };

    // static Bindings _bindings;

    static GDExtensionPropertyInfo _convert_prop_info(PropertyInfo &p_info) {
        GDExtensionPropertyInfo info = {};
        info.name = &p_info.name;
        info.class_name = &p_info.class_name;
        info.hint = p_info.hint;
        info.hint_string = &p_info.hint_string;
        info.usage = p_info.usage;
        return info;
    }

    static GDExtensionMethodInfo _convert_method_info(MethodInfo info) {
        GDExtensionMethodInfo minfo = {};
        minfo.name = &info.name;
        minfo.return_value = _convert_prop_info(info.return_val);
        minfo.flags = info.flags;
        minfo.id = info.id;
        minfo.argument_count = info.arguments.size();
        minfo.arguments = (GDExtensionPropertyInfo*) std::malloc(minfo.argument_count * sizeof(GDExtensionPropertyInfo));
        for (int i = 0; i < minfo.argument_count; i++) {
            minfo.arguments[i] = _convert_prop_info(info.arguments[i]);
        }
        minfo.default_argument_count = info.default_arguments.size();
        for (int i = 0; i < minfo.default_argument_count; i++) {
            minfo.default_arguments[i] = &info.default_arguments[i];
        }
        return minfo;
    }

    static GDExtensionCallError _convert_call_err(SchemeCallError p_err) {
        GDExtensionCallError gde_err;
        gde_err.error = (GDExtensionCallErrorType)p_err.error;
        gde_err.argument = p_err.argument;
        gde_err.expected = p_err.expected;
        return gde_err;
    }

    static GDExtensionObjectPtr s_get_script(GDExtensionScriptInstanceDataPtr void_this) {
        auto script = ((reinterpret_cast<SchemeScriptInstance*>(void_this))->get_script());
        return script.ptr();
    }

    static void s_free(GDExtensionScriptInstanceDataPtr void_this) {
        delete reinterpret_cast<SchemeScriptInstance*>(void_this);
    }

    static GDExtensionBool s_has_method(GDExtensionScriptInstanceDataPtr p_instance, GDExtensionConstStringNamePtr p_name) {
        return reinterpret_cast<SchemeScriptInstance*>(p_instance)->has_method(*reinterpret_cast<const StringName*>(p_name));
    }

    static void s_call(
            GDExtensionScriptInstanceDataPtr p_instance,
            GDExtensionConstStringNamePtr p_method,
            const GDExtensionConstVariantPtr *p_args,
            GDExtensionInt p_argument_count,
            GDExtensionVariantPtr r_return,
            GDExtensionCallError *r_error
        ) {

        auto args = reinterpret_cast<const Variant*>(p_args);
        auto method_name = *reinterpret_cast<const StringName*>(p_method);
        auto ret_ptr = reinterpret_cast<Variant*>(r_return);
        SchemeCallError ret_err;
        *ret_ptr = reinterpret_cast<SchemeScriptInstance*>(p_instance)->callp(
            method_name, &args, p_argument_count, ret_err);
        *r_error = _convert_call_err(ret_err);
    }

    static GDExtensionBool s_set(
        GDExtensionScriptInstanceDataPtr p_instance,
        GDExtensionConstStringNamePtr p_name,
        GDExtensionConstVariantPtr p_value) {
        return reinterpret_cast<SchemeScriptInstance*>(p_instance)->set(
            *reinterpret_cast<const StringName*>(p_name),
            *reinterpret_cast<const Variant*>(p_value));
    }

    static GDExtensionBool s_get(
        GDExtensionScriptInstanceDataPtr p_instance,
        GDExtensionConstStringNamePtr p_name,
        GDExtensionVariantPtr r_ret) {
        auto ret = *reinterpret_cast<Variant*>(r_ret);
        return reinterpret_cast<SchemeScriptInstance*>(p_instance)->get(
            *reinterpret_cast<const StringName*>(p_name),
            ret
        );
    }

    static const GDExtensionPropertyInfo* s_get_property_list(
        GDExtensionScriptInstanceDataPtr p_instance,
        uint32_t *r_count) {

        List<PropertyInfo> prop_info;
        reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_property_list(&prop_info);
        int count = prop_info.size();
        GDExtensionPropertyInfo* gde_info = (GDExtensionPropertyInfo*) std::malloc( sizeof(GDExtensionPropertyInfo));
        for (int i = 0; i < count; i++) {
            gde_info[i] = _convert_prop_info(prop_info[i]);
        }
        *r_count = count;
        return gde_info;
    }

    static void s_free_property_list(GDExtensionScriptInstanceDataPtr p_instance, const GDExtensionPropertyInfo *p_list) {
        delete p_list;
    }

    static GDExtensionVariantType s_get_property_type(
        GDExtensionScriptInstanceDataPtr p_instance,
        GDExtensionConstStringNamePtr p_name,
        GDExtensionBool *r_is_valid) {

        auto prop_name = *reinterpret_cast<const StringName*>(p_name);
        auto is_valid = reinterpret_cast<bool*>(r_is_valid);
        auto prop_type = reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_property_type(prop_name, is_valid);
        return (GDExtensionVariantType)prop_type;
    }

    static void s_notification(GDExtensionScriptInstanceDataPtr p_instance, int32_t p_what) {
        reinterpret_cast<SchemeScriptInstance*>(p_instance)->notification(p_what);
    }

    static GDExtensionScriptLanguagePtr s_get_language(GDExtensionScriptInstanceDataPtr p_instance) {
        UtilityFunctions::print_verbose("s_get_language invoke");
        return reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_language();
    }

    static GDExtensionObjectPtr s_get_owner(GDExtensionScriptInstanceDataPtr p_instance) {
        return reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_owner();
    }

    static const GDExtensionMethodInfo* s_get_method_list(GDExtensionScriptInstanceDataPtr p_instance, uint32_t *r_count) {
        List<MethodInfo> p_list;
        reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_method_list(&p_list);

        auto sz = (p_list.size());
        GDExtensionMethodInfo* m_info = (GDExtensionMethodInfo*) std::malloc(sz * sizeof(GDExtensionMethodInfo));
        for (int i = 0; i < sz; i++) {
            m_info[i] = _convert_method_info(p_list[i]);
        }
        *r_count = (uint32_t)sz;
        return m_info;
    }

    static void s_free_method_list(GDExtensionScriptInstanceDataPtr p_instance, const GDExtensionMethodInfo *p_list) {
        delete p_list;
    }

};

