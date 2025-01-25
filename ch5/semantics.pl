/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(semantics,
    [ is_valid_ast/1,   % Succeeds if Ast is a validated AST.
      validate/2        % Transforms the input program into a validated one.
    ]).
:- use_module(parser, [is_ast/1]).


/** <module> Parser
 
Semantic analysis for Chapter 5 of "Writing a C Compiler".

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
    resolve_program(Program0, Program).


%-------------------------%
%   VARIABLE RESOLUTION   %
%-------------------------%

%!  resolve_program(+Program, -ValProgram)
%
%   Resolves the variables in the program Program, producing ValProgram.
%   ValProgram is identical to Program except:
%   * There are no duplicate variables.
%   * Variables have a unique global name.
%   * Increment operators have a variable as an argument.
%   * Assignment operators have a variable as left argument.
%
%   @throws duplicated_var(Name) if a variable is duplicated.
%   @throws undefined_variable(Name) if a variable is used without being defined.
%   @throws invalid_lvalue(Expression) if an expression is used where a var is required.

resolve_program(program(FunDef), program(ValFunDef)) :-
    reset_gensym,
    empty_assoc(S0),
    resolve_function(FunDef, ValFunDef, S0, _S).

resolve_function(function(Name, Body), function(Name, ValBody), S0, S) :-
    foldl(resolve_item, Body, ValBody, S0, S).

resolve_item(d(Decl), d(ValDecl), S0, S) :-
    resolve_decl(Decl, ValDecl, S0, S).
resolve_item(s(Stmt), s(ValStmt), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S).

%!  resolve_decl(+Decl, -ValDecl, +SIn, -SOut)
%
%   Resolves the variables in a declaration.

resolve_decl(declaration(Name, Init), declaration(UniqueName, ValInit), S0, S) :-
    (   get_assoc(Name, S0, _)
    ->  throw(duplicated_var(Name))
    ;   mk_varname(Name, UniqueName),
        put_assoc(Name, S0, UniqueName, S1),
        (   Init = none
        ->  ValInit = none, S = S1
        ;   resolve_exp(Init, ValInit, S1, S)
        )
    ).

%!  resolve_stmt(+Stmt, -ValStmt, +SIn, -SOut)
%
%   Resolves the variables in a statement.

resolve_stmt(return(Exp), return(ValExp), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S).
resolve_stmt(expression(Exp), expression(ValExp), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S).
resolve_stmt(null, null, S, S).

%!  resolve_exp(+Exp, -ValExp, +SIn, -SOut)
%
%   Resolves the variables in an expression.

resolve_exp(constant(Int), constant(Int), S, S).
resolve_exp(var(Name), var(UniqueName), S, S) :-
    (   get_assoc(Name, S, UniqueName)
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

%!  mk_varname(+VarName, -UniqueName)
%
%   Generate a unique name for the given variable name.

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
    assertion(is_valid_ast(ProgramOut)),
    assertion(ProgramOut = program(function(main, [
        d(declaration('var.a.1', constant(5))),
        d(declaration('var.b.2', none)),
        s(expression(assignment(var('var.b.2'), binary(subtract, var('var.a.1'), constant(3))))),
        s(return(var('var.b.2')))
    ]))).

:- end_tests(semantics).
