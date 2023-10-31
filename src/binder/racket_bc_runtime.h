#ifndef RACKET_BC_RUNTIME_H
#define RACKET_BC_RUNTIME_H

#include <godot_cpp/classes/object.hpp>

#include "scheme.h"

using namespace godot;

class RacketBCRuntime : public Object {
  GDCLASS(RacketBCRuntime, Object)

  static RacketBCRuntime *singleton;
  static Scheme_Env *root_scheme_env;
  static Scheme_Object *root_env_output;

protected:
  static void _bind_methods() {}

public:
  static RacketBCRuntime *get_singleton();

  RacketBCRuntime::RacketBCRuntime();
  RacketBCRuntime::~RacketBCRuntime();

  Scheme_Object *eval(Scheme_Object *p_app_list);
  Scheme_Object *eval_string(char *str);
  void eval_handle(Scheme_Object *p_app_list, Scheme_Object **result_or_exn,
                   bool *ok);

  template <typename... T> Scheme_Object *evalv(T... ts) {
    return eval(rkt_list(ts...));
  }

  template <typename... T>
  void *eval_handlev(Scheme_Object **result_or_exn, bool *ok, T... ts) {
    return eval_handle(rkt_list(ts...), result_or_exn, ok);
  }
};

#endif // RACKET_BC_RUNTIME_H
