/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(semantics, [validate/2]).


/** <module> Parser
 
Semantic analysis for Chapter 7 of "Writing a C Compiler".

*/

validate(program(FunDef), Program) :-
    validate_program(program(FunDef), Program),
    label_program(Program).


%-------------------------%
%   VARIABLE RESOLUTION   %
%-------------------------%

validate_program(program(FunDef), program(ValFunDef)) :-
    reset_gensym,
    empty_table(S0),
    validate_function(FunDef, ValFunDef, S0, _S).

validate_function(function(Name, Body), function(Name, ValBody), S0, S) :-
    validate_block(Body, ValBody, S0, S).

validate_block(block(Items), block(ValItems), S0, S0) :-
    table_push_scope(S0, S1),
    foldl(validate_item, Items, ValItems, S1, _).

validate_item(d(Decl), d(ValDecl), S0, S) :-
    validate_decl(Decl, ValDecl, S0, S).
validate_item(s(Stmt), s(ValStmt), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S).

validate_decl(declaration(Name, Exp), declaration(UniqueName, ValExp), S0, S) :-
    (   table_get_local_entry(S0, Name, _)
    ->  throw(duplicated_var(Name))
    ;   mk_varname(Name, UniqueName),
        table_add_entry(S0, Name, UniqueName, S1),
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
validate_stmt(compound(Block), compound(ValBlock), S0, S) :-
    validate_block(Block, ValBlock, S0, S).
validate_stmt(goto(Label), goto(Label), S, S).
validate_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S).
validate_stmt(null, null, S, S).

validate_exp(constant(Int), constant(Int), S, S).
validate_exp(var(Name), var(UniqueName), S, S) :-
    (   table_get_global_entry(S, Name, UniqueName)
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


empty_table([]).

table_push_scope(Table0, [Scope|Table0]) :-
    empty_assoc(Scope).

table_add_entry(Table0, Key, Value, Table) :-
    Table0 = [Scope0|Rest],
    put_assoc(Key, Scope0, Value, Scope1),
    Table = [Scope1|Rest].

table_get_local_entry(Table, Key, Value) :-
    Table = [Scope|_],
    get_assoc(Key, Scope, Value).

table_get_global_entry(Table, Key, Value) :-
    Table = [Scope|Rest],
    (   get_assoc(Key, Scope, Value)
    ->  true
    ;   table_get_global_entry(Rest, Key, Value)
    ).


%----------------------%
%   LABEL RESOLUTION   %
%----------------------%

label_program(program(FunDef)) :-
    label_function(FunDef).

label_function(function(_Name, Body)) :-
    S0 = state([], []),
    label_block(Body, S0, state(Labels, Gotos)),
    subtract(Gotos, Labels, Undefined),
    (   Undefined = [L|_]
    ->  throw(undefined_label(L))
    ;   true
    ).

label_block(block(Items), S0, S) :-
    foldl(label_item, Items, S0, S).

label_item(d(_Decl), S, S).
label_item(s(Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).

label_stmt(return(_Exp), S, S).
label_stmt(expression(_Exp), S, S).
label_stmt(if(_Cond, Then, Else), S0, S) :-
    label_stmt(Then, S0, S1),
    (   Else = none
    ->  S = S1
    ;   label_stmt(Else, S1, S)
    ).
label_stmt(compound(Block), S0, S) :-
    label_block(Block, S0, S).
label_stmt(goto(Label), S0, S) :-
    S0 = state(Labels, Gotos),
    (   memberchk(Label, Gotos)
    ->  true
    ;   S = state(Labels, [Label|Gotos])
    ).
label_stmt(labelled(Label, Stmt), S0, S) :-
    S0 = state(Labels, Gotos),
    (   memberchk(Label, Labels)
    ->  throw(duplicated_label(Label))
    ;   S1 = state([Label|Labels], Gotos),
        label_stmt(Stmt, S1, S)
    ).
label_stmt(null, S, S).


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(semantics).

test(validate) :-
    ProgramIn = program(function(main, block([
        d(declaration(a, constant(5))),
        d(declaration(b, none)),
        s(expression(assignment(var(b), binary(subtract, var(a), constant(3))))),
        s(return(var(b)))
    ]))),
    validate(ProgramIn, ProgramOut),
    ProgramOut = program(function(main, block([
        d(declaration('var.a.1', constant(5))),
        d(declaration('var.b.2', none)),
        s(expression(assignment(var('var.b.2'), binary(subtract, var('var.a.1'), constant(3))))),
        s(return(var('var.b.2')))
    ]))).

:- end_tests(semantics).