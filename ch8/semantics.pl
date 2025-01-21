/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(semantics,
    [ is_valid_ast/1,   % Succeeds if Ast is a validated AST.
      validate/2        % Transforms the input program into a validated one.
    ]).
:- use_module(parser, [is_ast/1]).
:- use_module(scoped_table).


/** <module> Parser
 
Semantic analysis for Chapter 8 of "Writing a C Compiler".

*/

%!  is_valid_ast(+Ast)
%
%   Succeeds if Ast is a validated AST.

is_valid_ast(Ast) :-
    is_valid_program_ast(Ast).

is_valid_program_ast(program(FunctionDefinition)) :-
    is_valid_fundef_ast(FunctionDefinition).

is_valid_fundef_ast(function(Name, Body)) :-
    atom(Name),
    is_valid_block_ast(Body).

is_valid_block_ast(block(Items)) :-
    forall(member(Item, Items), is_valid_block_item_ast(Item)).

is_valid_block_item_ast(s(Statement)) :-
    is_valid_statement_ast(Statement).
is_valid_block_item_ast(d(Declaration)) :-
    is_valid_declaration_ast(Declaration).

is_valid_declaration_ast(declaration(Name, Item)) :-
    atom(Name),
    is_valid_opt_exp_ast(Item).

is_valid_statement_ast(return(Exp)) :-
    is_valid_exp_ast(Exp).
is_valid_statement_ast(expression(Exp)) :-
    is_valid_exp_ast(Exp).
is_valid_statement_ast(if(Condition, Then, Else)) :-
    is_valid_exp_ast(Condition),
    is_valid_statement_ast(Then),
    (   Else = none
    ->  true
    ;   is_valid_statement_ast(Else)
    ).
is_valid_statement_ast(compound(Block)) :-
    is_valid_block_ast(Block).
is_valid_statement_ast(break(Label)) :-
    atom(Label).
is_valid_statement_ast(continue(Label)) :-
    atom(Label).
is_valid_statement_ast(while(Exp, Stmt, Label)) :-
    is_valid_exp_ast(Exp),
    is_valid_statement_ast(Stmt),
    atom(Label).
is_valid_statement_ast(do_while(Stmt, Exp, Label)) :-
    is_valid_statement_ast(Stmt),
    is_valid_exp_ast(Exp),
    atom(Label).
is_valid_statement_ast(for(Init, Cond, Post, Stmt, Label)) :-
    is_valid_for_init_ast(Init),
    is_valid_opt_exp_ast(Cond),
    is_valid_opt_exp_ast(Post),
    is_valid_statement_ast(Stmt),
    atom(Label).
is_valid_statement_ast(switch(Exp, Stmt, Label, Cases)) :-
    is_valid_exp_ast(Exp),
    is_valid_statement_ast(Stmt),
    atom(Label),
    forall(member(Case, Cases), is_valid_case_ast(Case)).
is_valid_statement_ast(case(Exp, Stmt, Label)) :-
    is_valid_exp_ast(Exp),
    is_valid_statement_ast(Stmt),
    atom(Label).
is_valid_statement_ast(default(Stmt, Label)) :-
    is_valid_statement_ast(Stmt),
    atom(Label).
is_valid_statement_ast(goto(Label)) :-
    atom(Label).
is_valid_statement_ast(labelled(Label, Stmt)) :-
    atom(Label),
    is_valid_statement_ast(Stmt).
is_valid_statement_ast(null).

is_valid_for_init_ast(init_decl(Decl)) :-
    is_valid_declaration_ast(Decl).
is_valid_for_init_ast(init_exp(Exp)) :-
    is_valid_opt_exp_ast(Exp).

is_valid_case_ast(case(Exp, Label)) :-
    is_valid_exp_ast(Exp),
    atom(Label).
is_valid_case_ast(default(Label)) :-
    atom(Label).

is_valid_exp_ast(constant(Value)) :-
    integer(Value).
is_valid_exp_ast(var(Id)) :-
    atom(Id).
is_valid_exp_ast(assignment(Left, Right)) :-
    is_valid_exp_ast(Left),
    is_valid_exp_ast(Right).
is_valid_exp_ast(unary(Op, Exp)) :-
    is_valid_unary_op(Op),
    is_valid_exp_ast(Exp).
is_valid_exp_ast(binary(Op, Left, Right)) :-
    is_valid_bin_op(Op),
    is_valid_exp_ast(Left),
    is_valid_exp_ast(Right).
is_valid_exp_ast(conditional(Condition, Left, Right)) :-
    is_valid_exp_ast(Condition),
    is_valid_exp_ast(Left),
    is_valid_exp_ast(Right).

is_valid_opt_exp_ast(Exp) :-
    (   Exp = none
    ->  true
    ;   is_valid_exp_ast(Exp)
    ).

is_valid_unary_op(negate).
is_valid_unary_op(complement).
is_valid_unary_op(not).
is_valid_unary_op(pre_incr).
is_valid_unary_op(pre_decr).
is_valid_unary_op(post_incr).
is_valid_unary_op(post_decr).

is_valid_bin_op(multiply).
is_valid_bin_op(divide).
is_valid_bin_op(remainder).
is_valid_bin_op(add).
is_valid_bin_op(subtract).
is_valid_bin_op(lshift).
is_valid_bin_op(rshift).
is_valid_bin_op(less_than).
is_valid_bin_op(less_eq).
is_valid_bin_op(greater_than).
is_valid_bin_op(greater_eq).
is_valid_bin_op(equal).
is_valid_bin_op(not_equal).
is_valid_bin_op(bit_and).
is_valid_bin_op(bit_xor).
is_valid_bin_op(bit_or).
is_valid_bin_op(and).
is_valid_bin_op(or).
is_valid_bin_op(cond).

%!  validate(+Program, -ValidatedProgram)
%
%   Transforms the input program into a validated one.

validate(Program0, Program) :-
    assertion(is_ast(Program0)),
    resolve_program(Program0, Program1),
    label_program(Program1, Program2),
    loop_program(Program2, Program3),
    gather_program(Program3, Program).


%-------------------------%
%   VARIABLE RESOLUTION   %
%-------------------------%

%!  resolve_program(+Program, -ValProgram)
%
%   Resolves the variables in the program Program, producing ValProgram.
%   ValProgram is identical to Program except:
%   * There are no duplicate variables (same name in same scope).
%   * Variables have a unique global name.
%   * Increment operators have a variable as an argument.
%   * Assignment operators have a variable as left argument.
%
%   @throws duplicated_var(Name) if a variable is duplicated.
%   @throws undefined_variable(Name) if a variable is used without being defined.
%   @throws invalid_lvalue(Expression) if an expression is used where a var is required.

resolve_program(program(FunDef), program(ValFunDef)) :-
    reset_gensym,
    empty_table(S0),
    resolve_function(FunDef, ValFunDef, S0, _S).

resolve_function(function(Name, Body), function(Name, ValBody), S0, S) :-
    resolve_block(Body, ValBody, S0, S).

resolve_block(block(Items), block(ValItems), S0, S0) :-
    table_push_scope(S0, S1),   % Each block has its own scope.
    foldl(resolve_item, Items, ValItems, S1, _).

resolve_item(d(Decl), d(ValDecl), S0, S) :-
    resolve_decl(Decl, ValDecl, S0, S).
resolve_item(s(Stmt), s(ValStmt), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S).

%!  resolve_decl(+Decl, -ValDecl, +SIn, -SOut)
%
%   Resolves the variables in a declaration.

resolve_decl(declaration(Name, Exp), declaration(UniqueName, ValExp), S0, S) :-
    (   table_get_local_entry(S0, Name, _)
    ->  throw(duplicated_var(Name))
    ;   mk_varname(Name, UniqueName),
        table_add_entry(S0, Name, UniqueName, S1),
        resolve_exp_opt(Exp, ValExp, S1, S)
    ).

%!  resolve_stmt(+Stmt, -ValStmt, +SIn, -SOut)
%
%   Resolves the variables in a statement.

resolve_stmt(return(Exp), return(ValExp), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S).
resolve_stmt(expression(Exp), expression(ValExp), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S).
resolve_stmt(if(Cond, Then, Else), if(ValCond, ValThen, ValElse), S0, S) :-
    resolve_exp(Cond, ValCond, S0, S1),
    resolve_stmt(Then, ValThen, S1, S2),
    (   Else = none
    ->  S = S2, 
        ValElse = none
    ;   resolve_stmt(Else, ValElse, S2, S)
    ).
resolve_stmt(compound(Block), compound(ValBlock), S0, S) :-
    resolve_block(Block, ValBlock, S0, S).
resolve_stmt(break, break, S, S).
resolve_stmt(continue, continue, S, S).
resolve_stmt(while(Exp, Stmt), while(ValExp, ValStmt), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S1),
    resolve_stmt(Stmt, ValStmt, S1, S).
resolve_stmt(do_while(Stmt, Exp), do_while(ValStmt, ValExp), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S1),
    resolve_exp(Exp, ValExp, S1, S).    
resolve_stmt(for(Init, Cond, Post, Stmt), for(ValInit, ValCond, ValPost, ValStmt), S0, S0) :-
    table_push_scope(S0, S1),   % for has its own scope with the vars declared in Init.
    resolve_for_init(Init, ValInit, S1, S2),
    resolve_exp_opt(Cond, ValCond, S2, S3),
    resolve_exp_opt(Post, ValPost, S3, S4),
    resolve_stmt(Stmt, ValStmt, S4, _S).
resolve_stmt(switch(Exp, Stmt), switch(ValExp, ValStmt), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S1),
    resolve_stmt(Stmt, ValStmt, S1, S).
resolve_stmt(case(Exp, Stmt), case(ValExp, ValStmt), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S1),
    resolve_stmt(Stmt, ValStmt, S1, S).
resolve_stmt(default(Stmt), default(ValStmt), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S).
resolve_stmt(goto(Label), goto(Label), S, S).
resolve_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S).
resolve_stmt(null, null, S, S).

resolve_for_init(init_decl(Decl), init_decl(ValDecl), S0, S) :-
    resolve_decl(Decl, ValDecl, S0, S).
resolve_for_init(init_exp(Exp), init_exp(ValExp), S0, S) :-
    resolve_exp_opt(Exp, ValExp, S0, S).

%!  resolve_exp(+Exp, -ValExp, +SIn, -SOut)
%
%   Resolves the variables in an expression.

resolve_exp(constant(Int), constant(Int), S, S).
resolve_exp(var(Name), var(UniqueName), S, S) :-
    (   table_get_global_entry(S, Name, UniqueName)
    ->  true
    ;   throw(undefined_variable(Name))
    ).
resolve_exp(unary(Op, Val), unary(Op, ValVal), S0, S) :-
    (   memberchk(Op, [pre_incr, pre_decr, post_incr, post_decr]),
        Val \= var(_)
    ->  throw(invalid_lvalue(Val))
    ;   true
    ),
    resolve_exp(Val, ValVal, S0, S).
resolve_exp(binary(Op, Left, Right), binary(Op, ValLeft, ValRight), S0, S) :-
    resolve_exp(Left, ValLeft, S0, S1),
    resolve_exp(Right, ValRight, S1, S).
resolve_exp(assignment(Var, Val), assignment(ValVar, ValVal), S0, S) :-
    (   Var \= var(_)
    ->  throw(invalid_lvalue(Var))
    ;   resolve_exp(Var, ValVar, S0, S1),
        resolve_exp(Val, ValVal, S1, S)
    ).
resolve_exp(conditional(Cond, Then, Else), conditional(ValCond, ValThen, ValElse), S0, S) :-
    resolve_exp(Cond, ValCond, S0, S1),
    resolve_exp(Then, ValThen, S1, S2),
    resolve_exp(Else, ValElse, S2, S).

resolve_exp_opt(none, none, S, S) :-
    !.
resolve_exp_opt(Exp, ValExp, S0, S) :-
    Exp \= none,
    resolve_exp(Exp, ValExp, S0, S).

%!  mk_varname(+VarName, -UniqueName)
%
%   Generate a unique name for the given variable name.

mk_varname(Name, UniqueName) :-
    gensym('var.', Unique),
    atom_concat('var.', Id, Unique),
    atomic_list_concat(['var', Name, Id], '.', UniqueName).


%----------------------%
%   LABEL RESOLUTION   %
%----------------------%

%!  label_program(+Program, -ValProgram)
%
%   Checks the labels in the program Program, producing ValProgram.
%   ValProgram is identical to Program except:
%   * There are no duplicate labels.
%   * All gotos point to a valid label.
%
%   @throws undefined_label(Name) if a label is used without being defined.
%   @throws duplicated_label(Name) if the same label is used more than once in the same function.

label_program(program(FunDef), program(FunDef)) :-
    % No transformation right now, just checks.
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
    ->  S = S0
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
    assertion(is_valid_ast(ProgramOut)),
    assertion(ProgramOut = program(function(main, block([
        d(declaration('var.a.1', constant(5))),
        d(declaration('var.b.2', none)),
        s(expression(assignment(var('var.b.2'), binary(subtract, var('var.a.1'), constant(3))))),
        s(return(var('var.b.2')))
    ])))).

:- end_tests(semantics).