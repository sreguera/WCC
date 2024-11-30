/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(parser, [parse/2]).
:- use_module(dcg_utils).

/** <module> Parser
 
Parser for Chapter 5 of "Writing a C Compiler".

The parser parses a list of tokens into the AST of a program.
    * Program = program(FunctionDefinition)
    * FunctionDefinition = function(Name:atom, Body:[block_item])
    * BlockItem = s(Statement) | d(Declaration)
    * Declaration = declaration(Name, Exp?)
    * Statement = return(Exp) | expression(Exp) | null
    * Exp = 
        | constant(Value:int)
        | var(Identifier)
        | unary(Unop, Exp)
        | binary(Binop, Exp, Exp)
        | assignment(Exp, Exp)
    * Unop = complement | negate | not
    * Binop = add | subtract | multiply | divide | remainder
        | bit_and | bit_or | bit_xor | lshift | rshift
        | and | or | equal | not_equal
        | less_than | less_eq | greater_than | greater_eq
*/

program(program(FunctionDefinition)) -->
    function_definition(FunctionDefinition).

function_definition(function(Name, Body)) -->
    [int, identifier(Name), '(', void, ')', '{'],
    sequence(block_item, Body),
    ['}'].

block_item(Item) -->
    (   statement(S),
        { Item = s(S) }
    ;   declaration(D),
        { Item = d(D) }
    ).

declaration(declaration(Name, Init)) -->
    [int, identifier(Name)],
    optional(decl_init(Init), {Init = none}),
    [';'].

decl_init(Exp) -->
    ['='],
    exp(Exp).

statement(Stmt) -->
    ( return(Stmt)
    ; exp_stmt(Stmt)
    ; null(Stmt)
    ).

return(return(Exp)) -->
    [return],
    exp(Exp),
    [';'].

exp_stmt(expression(Exp)) -->
    exp(Exp),
    [';'].

null(null) -->
    [';'].

exp(Exp) -->
    exp(0, Exp).

exp(MinPrec, Exp) -->
    factor(P),
    exp_cont(MinPrec, P, Exp).

exp_cont(MinPrec, Left, Exp) -->
    binary_op(assign, OpPrec),
    { OpPrec >= MinPrec },
    exp(OpPrec, Right),
    exp_cont(MinPrec, assignment(Left, Right), Exp).
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
bin_op('<',  less_than, 35).
bin_op('<=', less_eq,   35).
bin_op('>',  greater_than, 35).
bin_op('>=', greater_eq,   35).
bin_op('==', equal,     30).
bin_op('!=', not_equal, 30).
bin_op('&',  bit_and,   25).
bin_op('^',  bit_xor,   20).
bin_op('|',  bit_or,    15).
bin_op('&&', and,       10).
bin_op('||', or,        5).
bin_op('=',  assign,    1).

factor(Exp) -->
    ( constant(Exp)
    ; var(Exp)
    ; unary(Exp)
    ; paren(Exp)
    ).

constant(constant(Int)) -->
    [constant(Int)].

var(var(Id)) -->
    [identifier(Id)].

unary(unary(Op, Exp)) -->
    [T],
    { unary_op(T, Op)
    },
    factor(Exp).

unary_op('-', negate).
unary_op('~', complement).
unary_op('!', not).

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
    Program = program(function(main, [s(return(constant(2)))])).

test(parse2) :-
    lex("int main(void) { return ~(-2); }", Tokens),
    parse(Tokens, Program),
    Program = program(function(main, [s(return(unary(complement, unary(negate, constant(2)))))])).

test(parse3) :-
    lex("int main(void) { return 1 + 2; }", Tokens),
    parse(Tokens, Program),
    Program = program(function(main, [s(return(binary(add,constant(1),constant(2))))])).

test(parse5) :-
    lex("int main(void) { int a = 5; int b; b = a - 3; return b; }", Tokens),
    parse(Tokens, Program),
    Program = program(function(main, [
        d(declaration(a, constant(5))),
        d(declaration(b, none)),
        s(expression(assignment(var(b), binary(subtract, var(a), constant(3))))),
        s(return(var(b)))
    ])).

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

test(exp) :-
    lex("~2 + 3", Tokens),
    once(phrase(exp(Exp), Tokens)),
    Exp = binary(add, unary(complement, constant(2)), constant(3)).

test(exp) :-
    lex("2 & 3", Tokens),
    once(phrase(exp(Exp), Tokens)),
    Exp = binary(bit_and, constant(2), constant(3)).

test(exp) :-
    lex("a = b = 5", Tokens),
    once(phrase(exp(Exp), Tokens)),
    Exp = assignment(var(a), assignment(var(b), constant(5))).

:- end_tests(parser).
