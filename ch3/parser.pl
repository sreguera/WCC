/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(parser,
    [ is_ast/1,     % Succeeds if Ast is a valid AST.
      parse/2       % Parses a list of tokens into the AST of a program.
    ]).
:- use_module(library(dcg/high_order)).

/** <module> Parser
 
Parser for Chapter 3 of "Writing a C Compiler".

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
is_exp_ast(binary(Op, Left, Right)) :-
    bin_op(_, Op, _),
    is_exp_ast(Left),
    is_exp_ast(Right).

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

bin_op('*',  multiply,  50).
bin_op('/',  divide,    50).
bin_op('%',  remainder, 50).
bin_op('+',  add,       45).
bin_op('-',  subtract,  45).
bin_op('<<', lshift,    40).
bin_op('>>', rshift,    40).
bin_op('&',  bit_and,   25).
bin_op('^',  bit_xor,   20).
bin_op('|',  bit_or,    15).

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
    factor(Exp).

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
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, return(constant(2))))).

test(parse2) :-
    lex("int main(void) { return ~(-2); }", Tokens),
    parse(Tokens, Program),
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, return(unary(complement, unary(negate, constant(2))))))).

test(parse3) :-
    lex("int main(void) { return 1 + 2; }", Tokens),
    parse(Tokens, Program),
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, return(binary(add, constant(1), constant(2)))))).

test(exp) :-
    lex("2 + 3 * 4", Tokens),
    once(phrase(exp(Exp), Tokens)),
    assertion(is_exp_ast(Exp)),
    Exp = binary(add,
            constant(2), 
            binary(multiply, constant(3), constant(4))).

test(exp) :-
    lex("2 + 3 - 4", Tokens),
    once(phrase(exp(Exp), Tokens)),
    assertion(is_exp_ast(Exp)),
    Exp = binary(subtract,
            binary(add, constant(2), constant(3)),
            constant(4)).

test(exp) :-
    lex("~2 + 3", Tokens),
    once(phrase(exp(Exp), Tokens)),
    assertion(is_exp_ast(Exp)),
    Exp = binary(add, unary(complement, constant(2)), constant(3)).

test(exp) :-
    lex("2 & 3", Tokens),
    once(phrase(exp(Exp), Tokens)),
    assertion(is_exp_ast(Exp)),
    Exp = binary(bit_and, constant(2), constant(3)).

:- end_tests(parser).
