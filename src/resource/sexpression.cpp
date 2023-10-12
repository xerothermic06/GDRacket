#include "sexpression.h"


Variant SExpression::evaluate() {
    // TODO: probably needs RacketLanguage to be exposed as an engine singleton, figure out namespace situation
    return Variant();
}


Variant SExpression::get_expression() {
    return expression;
}


void SExpression::set_expression(String p_expression) {
    expression = p_expression;
}


void SExpression::_bind_methods() {
    ClassDB::bind_method(D_METHOD("evaluate"), &SExpression::evaluate);
    ADD_PROPERTY(PropertyInfo(Variant::STRING, "expression"), "set_expression", "get_expression");
}
