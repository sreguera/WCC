/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(parser, [parse/2]).
:- use_module(dcg_utils).

/** <module> Parser
 
Parser for Chapter 6 of "Writing a C Compiler".

The parser parses a list of tokens into the AST of a program.
    * Program = program(FunctionDefinition)
    * FunctionDefinition = function(Name:atom, Body:[block_item])
    * BlockItem = s(Statement) | d(Declaration)
    * Declaration = declaration(Name, Exp?)
    * Statement =
        | return(Exp)
        | expression(Exp)
        | if(Condition:exp, Then:stmt, Else:stmt?)
        | labelled(Name, Stmt)
        | goto(Name)
        | null
    * Exp = 
        | constant(Value:int)
        | var(Identifier)
        | unary(Unop, Exp)
        | binary(Binop, Exp, Exp)
        | assignment(Exp, Exp)
        | conditional(Exp, Exp, Exp)
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
    ; if(Stmt)
    ; labelled(Stmt)
    ; goto(Stmt)
    ; null(Stmt)
    ).

return(return(Exp)) -->
    [return],
    exp(Exp),
    [';'].

exp_stmt(expression(Exp)) -->
    exp(Exp),
    [';'].

if(if(Cond, Then, Else)) -->
    ['if', '('],
    exp(Cond),
    [')'],
    statement(Then),
    optional(else(Else), {Else = none}).

else(Stmt) -->
    ['else'],
    statement(Stmt).

labelled(labelled(Name, Stmt)) -->
    [identifier(Name), ':'],
    statement(Stmt).

goto(goto(Id)) -->
    [goto, identifier(Id), ';'].

null(null) -->
    [';'].

exp(Exp) -->
    exp(0, Exp).

exp(MinPrec, Exp) -->
    factor(P),
    exp_cont(MinPrec, P, Exp).

exp_cont(MinPrec, Left, Exp) -->
    binary_op(cond, OpPrec, right),
    { OpPrec >= MinPrec },
    exp(0, Middle),
    [':'],
    exp(OpPrec, Right),
    { ContExp = conditional(Left, Middle, Right) },
    exp_cont(MinPrec, ContExp, Exp).
exp_cont(MinPrec, Left, Exp) -->
    binary_op(Op, OpPrec, right),
    { Op \= cond, OpPrec >= MinPrec },
    exp(OpPrec, Right),
    { bin_exp(Op, Left, Right, ContExp) },
    exp_cont(MinPrec, ContExp, Exp).
exp_cont(MinPrec, Left, Exp) -->
    binary_op(Op, OpPrec, left),
    { OpPrec >= MinPrec,
      MinPrec1 is OpPrec + 1
    },
    exp(MinPrec1, Right),
    exp_cont(MinPrec, binary(Op, Left, Right), Exp).
exp_cont(_, Left, Left) -->
    [].

bin_exp(Op, Left, Right, Expr) :-
    (   assign_op(Op, AOp)
    ->  (   AOp = none
        ->  Expr = assignment(Left, Right)
        ;   Expr = assignment(Left, binary(AOp, Left, Right))
        )
    ;   Expr = binary(Op, Left, Right)
    ).

binary_op(Op, Prec, Assoc) -->
    [C],
    { bin_op(C, Op, Prec, Assoc)
    }.

bin_op('*',   multiply,         50, left).
bin_op('/',   divide,           50, left).
bin_op('%',   remainder,        50, left).
bin_op('+',   add,              45, left).
bin_op('-',   subtract,         45, left).
bin_op('<<',  lshift,           40, left).
bin_op('>>',  rshift,           40, left).
bin_op('<',   less_than,        35, left).
bin_op('<=',  less_eq,          35, left).
bin_op('>',   greater_than,     35, left).
bin_op('>=',  greater_eq,       35, left).
bin_op('==',  equal,            30, left).
bin_op('!=',  not_equal,        30, left).
bin_op('&',   bit_and,          25, left).
bin_op('^',   bit_xor,          20, left).
bin_op('|',   bit_or,           15, left).
bin_op('&&',  and,              10, left).
bin_op('||',  or,                5, left).
bin_op('?',   cond,              3, right).
bin_op('=',   assign,            1, right).
bin_op('+=',  add_assign,        1, right).
bin_op('-=',  subtract_assign,   1, right).
bin_op('*=',  multiply_assign,   1, right).
bin_op('/=',  divide_assign,     1, right).
bin_op('%=',  remainder_assign,  1, right).
bin_op('&=',  and_assign,        1, right).
bin_op('|=',  or_assign,         1, right).
bin_op('^=',  xor_assign,        1, right).
bin_op('>>=', rshift_assign,     1, right).
bin_op('<<=', lshift_assign,     1, right).

assign_op(assign,           none).
assign_op(add_assign,       add).
assign_op(subtract_assign,  subtract).
assign_op(multiply_assign,  multiply).
assign_op(divide_assign,    divide).
assign_op(remainder_assign, remainder).
assign_op(and_assign,       bit_and).
assign_op(or_assign,        bit_or).
assign_op(xor_assign,       bit_xor).
assign_op(rshift_assign,    rshift).
assign_op(lshift_assign,    lshift).

factor(Exp) -->
    ( constant(Exp)
    ; var(Exp)
    ; unary(Exp)
    ; paren(Exp)
    ).

constant(PostfixExp) -->
    [constant(Int)],
    postfix_cont(constant(Int), PostfixExp).

var(PostfixExp) -->
    [identifier(Id)],
    postfix_cont(var(Id), PostfixExp).

unary(unary(Op, PostfixExp)) -->
    [T],
    { unary_op(T, Op)
    },
    factor(Exp),
    postfix_cont(Exp, PostfixExp).

unary_op('-', negate).
unary_op('~', complement).
unary_op('!', not).
unary_op('++', pre_incr).
unary_op('--', pre_decr).

paren(PostfixExp) -->
    ['('],
    exp(Exp), 
    [')'],
    postfix_cont(Exp, PostfixExp).

postfix_cont(In, Out) -->
    [T],
    { postfix_op(T, Op)
    },    
    postfix_cont(unary(Op, In), Out).
postfix_cont(In, In) -->
    [].

postfix_op('++', post_incr).
postfix_op('--', post_decr).


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

test(parse_if) :-
    lex("int main(void) { if (1) if (2) return 1; else return 2; }", Tokens),
    parse(Tokens, Program),
    Program = program(function(main, [
        s(if(constant(1),
            if(constant(2), return(constant(1)), return(constant(2))),
            none))
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

test(exp) :-
    lex("--a + -b++", Tokens),
    once(phrase(exp(Exp), Tokens)),
    Exp = binary(add, unary(pre_decr, var(a)), unary(negate, unary(post_incr, var(b)))).

test(decl) :-
    lex("int b = 3 + a++;", Tokens),
    once(phrase(declaration(Decl), Tokens)),
    Decl = declaration(b,binary(add,constant(3),unary(post_incr,var(a)))).

test(cond) :-
    lex("a ? b ? 1 : 2 : 3", Tokens),
    once(phrase(exp(Exp), Tokens)),
    Exp = conditional(var(a), conditional(var(b), constant(1), constant(2)), constant(3)).

test(cond_prec) :-
    lex("a || 0 ? 20 : 0", Tokens),
    once(phrase(exp(Exp), Tokens)),
    Exp = conditional(binary(or, var(a), constant(0)), constant(20), constant(0)).

:- end_tests(parser).
