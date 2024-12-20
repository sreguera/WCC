/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(semantics, [validate/2]).


/** <module> Parser
 
Semantic analysis for Chapter 8 of "Writing a C Compiler".

*/

validate(program(FunDef), Program) :-
    validate_program(program(FunDef), Program1),
    label_program(Program1),
    loop_program(Program1, Program2),
    gather_program(Program2, Program).


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

%!  validate_decl(+Decl, -ValDecl, +SIn, -SOut)
%
%   Validates the variables in a declaration.

validate_decl(declaration(Name, Exp), declaration(UniqueName, ValExp), S0, S) :-
    (   table_get_local_entry(S0, Name, _)
    ->  throw(duplicated_var(Name))
    ;   mk_varname(Name, UniqueName),
        table_add_entry(S0, Name, UniqueName, S1),
        validate_exp_opt(Exp, ValExp, S1, S)
    ).

%!  validate_stmt(+Stmt, -ValStmt, +SIn, -SOut)
%
%   Validates the variables in a statement.

validate_stmt(return(Exp), return(ValExp), S0, S) :-
    validate_exp(Exp, ValExp, S0, S).
validate_stmt(expression(Exp), expression(ValExp), S0, S) :-
    validate_exp(Exp, ValExp, S0, S).
validate_stmt(if(Cond, Then, Else), if(ValCond, ValThen, ValElse), S0, S) :-
    validate_exp(Cond, ValCond, S0, S1),
    validate_stmt(Then, ValThen, S1, S2),
    (   Else = none
    ->  S = S2, 
        ValElse = none
    ;   validate_stmt(Else, ValElse, S2, S)
    ).
validate_stmt(compound(Block), compound(ValBlock), S0, S) :-
    validate_block(Block, ValBlock, S0, S).
validate_stmt(break, break, S, S).
validate_stmt(continue, continue, S, S).
validate_stmt(while(Exp, Stmt), while(ValExp, ValStmt), S0, S) :-
    validate_exp(Exp, ValExp, S0, S1),
    validate_stmt(Stmt, ValStmt, S1, S).
validate_stmt(do_while(Stmt, Exp), do_while(ValStmt, ValExp), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S1),
    validate_exp(Exp, ValExp, S1, S).    
validate_stmt(for(Init, Cond, Post, Stmt), for(ValInit, ValCond, ValPost, ValStmt), S0, S0) :-
    table_push_scope(S0, S1),
    validate_for_init(Init, ValInit, S1, S2),
    validate_exp_opt(Cond, ValCond, S2, S3),
    validate_exp_opt(Post, ValPost, S3, S4),
    validate_stmt(Stmt, ValStmt, S4, _S).
validate_stmt(switch(Exp, Stmt), switch(ValExp, ValStmt), S0, S) :-
    validate_exp(Exp, ValExp, S0, S1),
    validate_stmt(Stmt, ValStmt, S1, S).
validate_stmt(case(Exp, Stmt), case(ValExp, ValStmt), S0, S) :-
    validate_exp(Exp, ValExp, S0, S1),
    validate_stmt(Stmt, ValStmt, S1, S).
validate_stmt(default(Stmt), default(ValStmt), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S).
validate_stmt(goto(Label), goto(Label), S, S).
validate_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S).
validate_stmt(null, null, S, S).

validate_for_init(init_decl(Decl), init_decl(ValDecl), S0, S) :-
    validate_decl(Decl, ValDecl, S0, S).
validate_for_init(init_exp(Exp), init_exp(ValExp), S0, S) :-
    validate_exp_opt(Exp, ValExp, S0, S).

%!  validate_exp(+Exp, -ValExp, +SIn, -SOut)
%
%   Validates the variables in an expression.

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

validate_exp_opt(none, none, S, S) :-
    !.
validate_exp_opt(Exp, ValExp, S0, S) :-
    Exp \= none,
    validate_exp(Exp, ValExp, S0, S).

mk_varname(Name, UniqueName) :-
    gensym('var.', Unique),
    atom_concat('var.', Id, Unique),
    atomic_list_concat(['var', Name, Id], '.', UniqueName).


%------------------%
%   SCOPED TABLE   %
%------------------%

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
label_stmt(break, S, S).
label_stmt(continue, S, S).
label_stmt(while(_Exp, Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
label_stmt(do_while(Stmt, _Exp), S0, S) :-
    label_stmt(Stmt, S0, S).    
label_stmt(for(_Init, _Cond, _Post, Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
label_stmt(switch(_Exp, Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
label_stmt(case(_Exp, Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
label_stmt(default(Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
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


%--------------------%
%   LOOP LABELLING   %
%--------------------%

loop_program(program(FunDef), program(ValFunDef)) :-
    reset_gensym,
    loop_function(FunDef, ValFunDef).

loop_function(function(Name, Body), function(Name, ValBody)) :-
    loop_block(Body, ValBody, []).

loop_block(block(Items), block(ValItems), S) :-
    maplist(loop_item_1(S), Items, ValItems).

loop_item_1(S, Item0, Item) :-
    loop_item(Item0, Item, S).

loop_item(d(Decl), d(Decl), _).
loop_item(s(Stmt), s(ValStmt), S) :-
    loop_stmt(Stmt, ValStmt, S).

loop_stmt(return(Exp), return(Exp), _).
loop_stmt(expression(Exp), expression(Exp), _).
loop_stmt(if(Cond, Then, Else), if(Cond, ValThen, ValElse), S) :-
    loop_stmt(Then, ValThen, S),
    (   Else = none
    ->  ValElse = none
    ;   loop_stmt(Else, ValElse, S)
    ).
loop_stmt(compound(Block), compound(ValBlock), S) :-
    loop_block(Block, ValBlock, S).
loop_stmt(break, break(Label), S) :-
    (   S = [loop(Label)|_]
    ->  true
    ;   S = [switch(Label)|_]
    ->  true
    ;   throw(break_outside_loop)
    ).
loop_stmt(continue, continue(Label), S) :-
    (   memberchk(loop(Label), S)
    ->  true
    ;   throw(continue_outside_loop)
    ).
loop_stmt(while(Exp, Stmt), while(Exp, ValStmt, Label), S) :-
    gensym(loop, Label),
    loop_stmt(Stmt, ValStmt, [loop(Label)|S]).
loop_stmt(do_while(Stmt, Exp), do_while(ValStmt, Exp, Label), S) :-
    gensym(loop, Label),
    loop_stmt(Stmt, ValStmt, [loop(Label)|S]).    
loop_stmt(for(Init, Cond, Post, Stmt), for(Init, Cond, Post, ValStmt, Label), S) :-
    gensym(loop, Label),
    loop_stmt(Stmt, ValStmt, [loop(Label)|S]).
loop_stmt(switch(Exp, Stmt), switch(Exp, ValStmt, Label), S) :-
    gensym(switch, Label),
    loop_stmt(Stmt, ValStmt, [switch(Label)|S]).
loop_stmt(case(Exp, Stmt), case(Exp, ValStmt, Label), S) :-
    (   memberchk(switch(_), S)
    ->  true
    ;   throw(case_without_switch)
    ),
    gensym(case, Label),
    loop_stmt(Stmt, ValStmt, S).
loop_stmt(default(Stmt), default(ValStmt, Label), S) :-
    (   memberchk(switch(_), S)
    ->  true
    ;   throw(default_without_switch)
    ),
    gensym(default, Label),
    loop_stmt(Stmt, ValStmt, S).
loop_stmt(goto(Label), goto(Label), _).
loop_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S) :-
    loop_stmt(Stmt, ValStmt, S).
loop_stmt(null, null, _).


%-------------------------%
%   GATHER SWITCH CASES   %
%-------------------------%

gather_program(program(FunDef), program(ValFunDef)) :-
    gather_function(FunDef, ValFunDef).

gather_function(function(Name, Body), function(Name, ValBody)) :-
    gather_block(Body, ValBody, [], []).

gather_block(block(Items), block(ValItems), S0, S) :-
    foldl(gather_item, Items, ValItems, S0, S).

gather_item(d(Decl), d(Decl), S, S).
gather_item(s(Stmt), s(ValStmt), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).

%!  gather_stmt(+Stmt, -ValStmt, +SIn, -SOut)
%
%   gathers the variables in a statement.

gather_stmt(return(Exp), return(Exp), S, S).
gather_stmt(expression(Exp), expression(Exp), S, S).
gather_stmt(if(Cond, Then, Else), if(Cond, ValThen, ValElse), S0, S) :-
    gather_stmt(Then, ValThen, S0, S1),
    (   Else = none
    ->  S = S1,
        ValElse = none
    ;   gather_stmt(Else, ValElse, S1, S)
    ).
gather_stmt(compound(Block), compound(ValBlock), S0, S) :-
    gather_block(Block, ValBlock, S0, S).
gather_stmt(break(Label), break(Label), S, S).
gather_stmt(continue(Label), continue(Label), S, S).
gather_stmt(while(Exp, Stmt, Label), while(Exp, ValStmt, Label), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(do_while(Stmt, Exp, Label), do_while(ValStmt, Exp, Label), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).    
gather_stmt(for(Init, Cond, Post, Stmt, Label), for(Init, Cond, Post, ValStmt, Label), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(switch(Exp, Stmt, Label), switch(Exp, ValStmt, Label, RS), S0, S0) :-
    gather_stmt(Stmt, ValStmt, [], S),
    switch_check(S),
    reverse(S, RS).
gather_stmt(case(Exp, Stmt, Label), case(Exp, ValStmt, Label), S0, [case(Exp, Label)|S]) :-
    (   Exp = constant(_)
    ->  true
    ;   throw(non_constant_case)
    ),
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(default(Stmt, Label), default(ValStmt, Label), S0, [default(Label)|S]) :-
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(goto(Label), goto(Label), S, S).
gather_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(null, null, S, S).

switch_check(S) :-
    maplist(without_label, S, S1),
    sort(0, @=<, S1, S2),
    clumped(S2, S3),
    sort(2, @>=, S3, S4),
    (   S4 = [X-Y|_],
        Y > 1
    ->  throw(duplicate_switch_clause(X))
    ;   true
    ).

without_label(default(_), default).
without_label(case(Exp, _), case(Exp)).


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