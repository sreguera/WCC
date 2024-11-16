/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(parser, [parse/2]).
:- use_module(dcg_utils).

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

exp(constant(Int)) -->
    [constant(Int)].

parse(Tokens, Program) :-
    once(phrase(program(Program), Tokens)).

:- begin_tests(parser).

:- use_module(lexer).

test(parse) :-
    scan("int main(void) { return 2; }", Tokens),
    parse(Tokens, Program),
    Program = program(function(main, return(constant(2)))).

:- end_tests(parser).