/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(semantics, [validate/2]).


/** <module> Parser
 
Semantic analysis for Chapter 6 of "Writing a C Compiler".

*/

validate(program(FunDef), program(ValFunDef)) :-
    reset_gensym,
    empty_assoc(S0),
    validate_function(FunDef, ValFunDef, S0, _S).

validate_function(function(Name, Body), function(Name, ValBody), S0, S) :-
    foldl(validate_item, Body, ValBody, S0, S).

validate_item(d(Decl), d(ValDecl), S0, S) :-
    validate_decl(Decl, ValDecl, S0, S).
validate_item(s(Stmt), s(ValStmt), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S).

validate_decl(declaration(Name, Exp), declaration(UniqueName, ValExp), S0, S) :-
    (   get_assoc(Name, S0, _)
    ->  throw(duplicated_var(Name))
    ;   mk_varname(Name, UniqueName),
        put_assoc(Name, S0, UniqueName, S1),
        (   Exp = none
        ->  ValExp = none, S = S1
        ;   validate_exp(Exp, ValExp, S1, S)
        )
    ).

validate_stmt(return(Exp), return(ValExp), S0, S) :-
    validate_exp(Exp, ValExp, S0, S).
validate_stmt(expression(Exp), expression(ValExp), S0, S) :-
    validate_exp(Exp, ValExp, S0, S).
validate_stmt(if(Cond, Then, Else), if(ValCond, ValThen, ValElse), S0, S) :-
    validate_exp(Cond, ValCond, S0, S1),
    validate_stmt(Then, ValThen, S1, S2),
    (   Else = none
    ->  S = S2
    ;   validate_stmt(Else, ValElse, S2, S)
    ).
validate_stmt(null, null, S, S).

validate_exp(constant(Int), constant(Int), S, S).
validate_exp(var(Name), var(UniqueName), S, S) :-
    (   get_assoc(Name, S, UniqueName)
    ->  true
    ;   throw(undefined_variable(Name))
    ).
validate_exp(unary(Op, Val), unary(Op, ValVal), S0, S) :-
    (   memberchk(Op, [pre_incr, pre_decr, post_incr, post_decr]),
        Val \= var(_)
    ->  throw(invalid_lvalue(Val))
    ;   true
    ),
    validate_exp(Val, ValVal, S0, S).
validate_exp(binary(Op, Left, Right), binary(Op, ValLeft, ValRight), S0, S) :-
    validate_exp(Left, ValLeft, S0, S1),
    validate_exp(Right, ValRight, S1, S).
validate_exp(assignment(Var, Val), assignment(ValVar, ValVal), S0, S) :-
    (   Var \= var(_)
    ->  throw(invalid_lvalue(Var))
    ;   validate_exp(Var, ValVar, S0, S1),
        validate_exp(Val, ValVal, S1, S)
    ).
validate_exp(conditional(Cond, Then, Else), conditional(ValCond, ValThen, ValElse), S0, S) :-
    validate_exp(Cond, ValCond, S0, S1),
    validate_exp(Then, ValThen, S1, S2),
    validate_exp(Else, ValElse, S2, S).

mk_varname(Name, UniqueName) :-
    gensym('var.', Unique),
    atom_concat('var.', Id, Unique),
    atomic_list_concat(['var', Name, Id], '.', UniqueName).
    

%-----------%
%   TESTS   %
%-----------%

:- begin_tests(semantics).

test(validate) :-
    ProgramIn = program(function(main, [
        d(declaration(a, constant(5))),
        d(declaration(b, none)),
        s(expression(assignment(var(b), binary(subtract, var(a), constant(3))))),
        s(return(var(b)))
    ])),
    validate(ProgramIn, ProgramOut),
    ProgramOut = program(function(main, [
        d(declaration('var.a.1', constant(5))),
        d(declaration('var.b.2', none)),
        s(expression(assignment(var('var.b.2'), binary(subtract, var('var.a.1'), constant(3))))),
        s(return(var('var.b.2')))
    ])).

:- end_tests(semantics).