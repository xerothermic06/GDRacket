#ifndef GDRACKET_BINDER_H
#define GDRACKET_BINDER_H

#include <thread>
#include <mutex>
#include <condition_variable>

#include "godot_cpp/templates/hash_map.hpp"
#include "godot_cpp/templates/hash_set.hpp"
#include "godot_cpp/classes/thread.hpp"

#include "racket_error.h"
#include "racket_binder.h"
#include "util/racket_classes.h"

#include "scheme.h"

// RacketBinder for RacketBC.
class RacketBCBinder : public RacketBinder {

    static RacketBCBinder* singleton;

    HashMap<uint32_t, Scheme_Env*> script_id_namespace_map;
    // Mapping from Script ID -> Racket symbol for module path
    HashMap<uint32_t, Scheme_Object*> script_id_class_map;
    // Mapping from Script ID -> class definition copy
    HashMap<uint32_t, GDClassDefinition> script_id_definition_map;
    // Mapping from ScriptInstance ID -> Racket object for instance
    HashMap<uint32_t, Scheme_Object*> instance_id_ctx_map;

protected:

public:
    enum EvalType {
        EVAL_TYPE_MODULE,
        EVAL_TYPE_GLOBAL,
    };

    RacketBCBinder();
    ~RacketBCBinder();

    static RacketBCBinder* get_singleton();

    Error create_definition(const RacketScript &script, GDClassDefinition& def);
    GDClassDefinition* get_definition(const RacketScript &script);
    void delete_definition(const RacketScript &p_script);

    int32_t get_member_line(RacketScriptInstance &p_target, const StringName &member);

    Error initialize_instance(RacketScriptInstance &p_target);
    Variant call(RacketScriptInstance &p_target, const String p_func_name, const Variant **p_args, int p_argcount, RacketCallError* r_error);
    bool set(RacketScriptInstance &p_target, const StringName &p_name, const Variant &p_value);
    bool get(const RacketScriptInstance &p_target, const StringName, Variant *r_ret) const;
    bool has_method(const RacketScriptInstance &p_target, const StringName &p_method) const;
    void free_instance(RacketScriptInstance &p_target);

private:
    Variant _extract_variant_with_collections(Scheme_Object* p_obj);
    Scheme_Object* _extract_scheme_value_with_collections(Variant p_value);
public:
    Dictionary hashtable_to_dictionary(Scheme_Object* p_obj);
    Array list_or_vector_to_array(Scheme_Object* p_obj);
    Scheme_Object* array_to_list(Array arr);
    Scheme_Object* dictionary_to_hashtable(Dictionary dict);

#ifdef TOOLS_ENABLED
    virtual const Dictionary &get_modified_api() const;
#endif
};

#endif // GDRACKET_BINDER_H
