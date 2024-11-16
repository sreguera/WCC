/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(parser, [parse/2]).
:- use_module(dcg_utils).

/** <module> Parser
 
Parser for Chapter 2 of "Writing a C Compiler".

The parser parses a list of tokens into the AST of a program.
    * Program = program(FunctionDefinition)
    * FunctionDefinition = function(Name:atom, Body)
    * Body = return(Exp)
    * Exp = 
        | constant(Value:int)
        | unary(Unop, Exp))
    * Unop = '~' | '-'
*/

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
    [Op],
    { unary_op(Op)
    },
    exp(Exp).

unary_op('-').
unary_op('~').

paren(Exp) -->
    ['('],
    exp(Exp),
    [')'].

%!  parse(+Tokens:[token], -Program) is det
%
%   Parses a list of tokens into the AST of a program.

parse(Tokens, Program) :-
    once(phrase(program(Program), Tokens)).

:- begin_tests(parser).

:- use_module(lexer).

test(parse) :-
    scan("int main(void) { return 2; }", Tokens),
    parse(Tokens, Program),
    Program = program(function(main, return(constant(2)))).

test(parse) :-
    scan("int main(void) { return ~(-2); }", Tokens),
    parse(Tokens, Program),
    writeln(Program),
    Program = program(function(main, return(unary('~', unary('-', constant(2)))))).

:- end_tests(parser).
