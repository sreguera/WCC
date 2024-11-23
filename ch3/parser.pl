/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(parser, [parse/2]).
:- use_module(dcg_utils).

/** <module> Parser
 
Parser for Chapter 3 of "Writing a C Compiler".

The parser parses a list of tokens into the AST of a program.
    * Program = program(FunctionDefinition)
    * FunctionDefinition = function(Name:atom, Body)
    * Body = return(Exp)
    * Exp = 
        | constant(Value:int)
        | unary(Unop, Exp)
        | binary(Binop, Exp, Exp)
    * Unop = complement | negate
    * Binop = add | subtract | multiply | divide | remainder
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
    exp(0, Exp).

exp(MinPrec, Exp) -->
    factor(P),
    exp_cont(MinPrec, P, Exp).

exp_cont(MinPrec, Left, Exp) -->
    binary_op(Op, OpPrec),
    { OpPrec >= MinPrec,
      MinPrec1 is OpPrec + 1
    },
    exp(MinPrec1, Right),
    exp_cont(MinPrec, binary(Op, Left, Right), Exp).
exp_cont(_, Left, Left) -->
    [].

binary_op(Op, Prec) -->
    [C],
    { bin_op(C, Op, Prec)
    }.

bin_op('+', add,       45).
bin_op('-', subtract,  45).
bin_op('*', multiply,  50).
bin_op('/', divide,    50).
bin_op('%', remainder, 50).

factor(Exp) -->
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

%!  parse(+Tokens:[token], -Program) is det
%
%   Parses a list of tokens into the AST of a program.

parse(Tokens, Program) :-
    once(phrase(program(Program), Tokens)).


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(parser).

:- use_module(lexer).

test(parse) :-
    lex("int main(void) { return 2; }", Tokens),
    parse(Tokens, Program),
    Program = program(function(main, return(constant(2)))).

test(parse2) :-
    lex("int main(void) { return ~(-2); }", Tokens),
    parse(Tokens, Program),
    Program = program(function(main, return(unary(complement, unary(negate, constant(2)))))).

test(parse3) :-
    lex("int main(void) { return 1 + 2; }", Tokens),
    parse(Tokens, Program),
    Program = program(function(main,return(binary(add,constant(1),constant(2))))).

test(exp) :-
    lex("2 + 3 * 4", Tokens),
    once(phrase(exp(Exp), Tokens)),
    Exp = binary(add,
            constant(2), 
            binary(multiply, constant(3), constant(4))).

test(exp) :-
    lex("2 + 3 - 4", Tokens),
    once(phrase(exp(Exp), Tokens)),
    Exp = binary(subtract,
            binary(add, constant(2), constant(3)),
            constant(4)).

:- end_tests(parser).
