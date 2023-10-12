#ifndef SEXPRESSION_H
#define SEXPRESSION_H

#include "godot_cpp/classes/resource.hpp"
#include "godot_cpp/variant/variant.hpp"

using namespace godot;

class SExpression : public Resource {
    GDCLASS(SExpression, Resource)
private:
    String expression;
protected:
    static void _bind_methods();
public:
    Variant get_expression();
    void set_expression(String p_expression);
    Variant evaluate();
};

#endif // SEXPRESSION_H
