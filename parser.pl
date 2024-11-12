:- module(parser, [parse/2]).
:- use_module(lexer).
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

parse(Source, Program) :-
    scan(Source, Tokens),
    once(phrase(program(Program), Tokens)).

:- begin_tests(parser).

test(parse) :-
    parse("int main(void) { return 2; }", Program),
    Program = program(function(main, return(constant(2)))).

:- end_tests(parser).