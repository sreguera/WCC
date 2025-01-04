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
    validate_program(Program0, Program).

validate_program(program(FunDef), program(ValFunDef)) :-
    reset_gensym,
    empty_assoc(S0),
    validate_function(FunDef, ValFunDef, S0, _S).

validate_function(function(Name, Body), function(Name, ValBody), S0, S) :-
    foldl(validate_item, Body, ValBody, S0, S).

validate_item(d(Decl), d(ValDecl), S0, S) :-
    validate_decl(Decl, ValDecl, S0, S).
validate_item(s(Stmt), s(ValStmt), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S).

%!  validate_decl(+Decl, -ValDecl, +SIn, -SOut)
%
%   Validates the variables in a declaration.

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

%!  validate_stmt(+Stmt, -ValStmt, +SIn, -SOut)
%
%   Validates the variables in a statement.

validate_stmt(return(Exp), return(ValExp), S0, S) :-
    validate_exp(Exp, ValExp, S0, S).
validate_stmt(expression(Exp), expression(ValExp), S0, S) :-
    validate_exp(Exp, ValExp, S0, S).
validate_stmt(null, null, S, S).

%!  validate_exp(+Exp, -ValExp, +SIn, -SOut)
%
%   Validates the variables in an expression.

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
