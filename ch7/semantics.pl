/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(semantics,
    [ is_valid_ast/1,   % Succeeds if Ast is a validated AST.
      validate/2        % Transforms the input program into a validated one.
    ]).
:- use_module(parser, [is_ast/1]).
:- use_module(scoped_table).


/** <module> Parser
 
Semantic analysis for Chapter 7 of "Writing a C Compiler".

*/

%!  is_valid_ast(+Ast)
%
%   Succeeds if Ast is a validated AST.

is_valid_ast(Ast) :-
    is_ast(Ast).

%!  validate(+Program, -ValidatedProgram)
%
%   Transforms the input program into a validated one.

validate(Program0, Program) :-
    assertion(is_ast(Program0)),
    resolve_program(Program0, Program1),
    label_program(Program1, Program).


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
        (   Exp = none
        ->  ValExp = none, S = S1
        ;   resolve_exp(Exp, ValExp, S1, S)
        )
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
resolve_stmt(goto(Label), goto(Label), S, S).
resolve_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S).
resolve_stmt(null, null, S, S).

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
    assertion(is_valid_ast(ProgramOut)),
    assertion(ProgramOut = program(function(main, block([
        d(declaration('var.a.1', constant(5))),
        d(declaration('var.b.2', none)),
        s(expression(assignment(var('var.b.2'), binary(subtract, var('var.a.1'), constant(3))))),
        s(return(var('var.b.2')))
    ])))).

:- end_tests(semantics).
