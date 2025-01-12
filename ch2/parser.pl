/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(parser,
    [ is_ast/1,     % Succeeds if Ast is a valid AST.
      parse/2       % Parses a list of tokens into the AST of a program.
    ]).
:- use_module(library(dcg/high_order)).

/** <module> Parser
 
Parser for Chapter 2 of "Writing a C Compiler".

The parser parses a list of tokens into the AST of a program.

*/

%!  is_ast(+Ast)
%
%   Succeeds if Ast is a valid AST.

is_ast(Ast) :-
    is_program_ast(Ast).

is_program_ast(program(FunctionDefinition)) :-
    is_fundef_ast(FunctionDefinition).

is_fundef_ast(function(Name, Body)) :-
    atom(Name),
    is_statement_ast(Body).

is_statement_ast(return(Exp)) :-
    is_exp_ast(Exp).

is_exp_ast(constant(Value)) :-
    integer(Value).
is_exp_ast(unary(Op, Exp)) :-
    unary_op(_, Op),
    is_exp_ast(Exp).

%!  parse(+Tokens:[token], -Program) is det
%
%   Parses a list of tokens into the AST of a program.

parse(Tokens, Program) :-
    once(phrase(program(Program), Tokens)).

%!  program(Program)//
%
%   Parses a C program and Program is the corresponding AST.

program(program(FunctionDefinition)) -->
    function_definition(FunctionDefinition).

function_definition(function(Name, Body)) -->
    [int, identifier(Name), '(', void, ')', '{'],
    statement(Body),
    ['}'].

statement(return(Exp)) -->
    [return],
    exp(Exp),
    [';'].

exp(Exp) -->
    ( constant(Exp)
    ; unary(Exp)
    ; paren(Exp)
    ).

constant(constant(Int)) -->
    [constant(Int)].

unary(unary(Op, Exp)) -->
    [T],
    { unary_op(T, Op)
    },
    exp(Exp).

unary_op('-', negate).
unary_op('~', complement).

paren(Exp) -->
    ['('],
    exp(Exp),
    [')'].


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(parser).

:- use_module(lexer).

test(parse) :-
    lex("int main(void) { return 2; }", Tokens),
    parse(Tokens, Program),
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, return(constant(2))))).

test(parse2) :-
    lex("int main(void) { return ~(-2); }", Tokens),
    parse(Tokens, Program),
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, return(unary(complement, unary(negate, constant(2))))))).

:- end_tests(parser).
