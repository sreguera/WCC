/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(parser,
    [ is_ast/1,     % Succeeds if Ast is a valid AST.
      parse/2       % Parses a list of tokens into the AST of a program.
    ]).
:- use_module(library(dcg/high_order)).

/** <module> Parser
 
Parser for Chapter 8 of "Writing a C Compiler".

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
    is_block_ast(Body).

is_block_ast(block(Items)) :-
    forall(member(Item, Items), is_block_item_ast(Item)).

is_block_item_ast(s(Statement)) :-
    is_statement_ast(Statement).
is_block_item_ast(d(Declaration)) :-
    is_declaration_ast(Declaration).

is_declaration_ast(declaration(Name, Init)) :-
    atom(Name),
    is_opt_exp_ast(Init).

is_statement_ast(return(Exp)) :-
    is_exp_ast(Exp).
is_statement_ast(expression(Exp)) :-
    is_exp_ast(Exp).
is_statement_ast(if(Condition, Then, Else)) :-
    is_exp_ast(Condition),
    is_statement_ast(Then),
    (   Else = none
    ->  true
    ;   is_statement_ast(Else)
    ).
is_statement_ast(compound(Block)) :-
    is_block_ast(Block).
is_statement_ast(break).
is_statement_ast(continue).
is_statement_ast(while(Exp, Stmt)) :-
    is_exp_ast(Exp),
    is_statement_ast(Stmt).
is_statement_ast(do_while(Stmt, Exp)) :-
    is_statement_ast(Stmt),
    is_exp_ast(Exp).
is_statement_ast(for(Init, Cond, Post, Stmt)) :-
    is_for_init_ast(Init),
    is_opt_exp_ast(Cond),
    is_opt_exp_ast(Post),
    is_statement_ast(Stmt).
is_statement_ast(switch(Exp, Stmt)) :-
    is_exp_ast(Exp),
    is_statement_ast(Stmt).
is_statement_ast(case(Exp, Stmt)) :-
    is_exp_ast(Exp),
    is_statement_ast(Stmt).
is_statement_ast(default(Stmt)) :-
    is_statement_ast(Stmt).
is_statement_ast(goto(Label)) :-
    atom(Label).
is_statement_ast(labelled(Label, Stmt)) :-
    atom(Label),
    is_statement_ast(Stmt).
is_statement_ast(null).

is_for_init_ast(init_decl(Decl)) :-
    is_declaration_ast(Decl).
is_for_init_ast(init_exp(Exp)) :-
    is_opt_exp_ast(Exp).

is_exp_ast(constant(Value)) :-
    integer(Value).
is_exp_ast(var(Id)) :-
    atom(Id).
is_exp_ast(assignment(Left, Right)) :-
    is_exp_ast(Left),
    is_exp_ast(Right).
is_exp_ast(unary(Op, Exp)) :-
    (   unary_op(_, Op)
    ->  true
    ;   postfix_op(_, Op)
    ),
    is_exp_ast(Exp).
is_exp_ast(binary(Op, Left, Right)) :-
    bin_op(_, Op, _, _),
    is_exp_ast(Left),
    is_exp_ast(Right).
is_exp_ast(conditional(Condition, Left, Right)) :-
    is_exp_ast(Condition),
    is_exp_ast(Left),
    is_exp_ast(Right).

is_opt_exp_ast(Exp) :-
    (   Exp = none
    ->  true
    ;   is_exp_ast(Exp)
    ).

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
    [int, identifier(Name), '(', void, ')'],
    block(Body).

block(block(Items)) -->
    ['{'],
    sequence(block_item, Items),
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
    ; compound(Stmt)
    ; break(Stmt)
    ; continue(Stmt)
    ; while(Stmt)
    ; do_while(Stmt)
    ; for(Stmt)
    ; switch(Stmt)
    ; case(Stmt)
    ; default(Stmt)
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

compound(compound(Block)) -->
    block(Block).

break(break) -->
    [break, ';'].
 
continue(continue) -->
    [continue, ';'].

while(while(Exp, Stmt)) -->
    [while, '('],
    exp(Exp),
    [')'],
    statement(Stmt).

do_while(do_while(Stmt, Exp)) -->
    [do],
    statement(Stmt),
    [while, '('],
    exp(Exp),
    [')', ';'].

for(for(Init, Cond, Post, Stmt)) -->
    [for, '('],
    for_init(Init),
    optional(exp(Cond), {Cond = none}),
    [';'],
    optional(exp(Post), {Post = none}),
    [')'],
    statement(Stmt).

for_init(init_decl(Decl)) -->
    declaration(Decl).
for_init(init_exp(Exp)) -->
    optional(exp(Exp), {Exp = none}),
    [';'].

switch(switch(Exp, Stmt)) -->
    [switch, '('],
    exp(Exp),
    [')'],
    statement(Stmt).

case(case(Exp, Stmt)) -->
    [case],
    exp(Exp),
    [':'],
    statement(Stmt).

default(default(Stmt)) -->
    [default, ':'],
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


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(parser).

:- use_module(lexer).

test(parse) :-
    lex("int main(void) { return 2; }", Tokens),
    parse(Tokens, Program),
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, block([s(return(constant(2)))])))).

test(parse2) :-
    lex("int main(void) { return ~(-2); }", Tokens),
    parse(Tokens, Program),
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, block([s(return(unary(complement, unary(negate, constant(2)))))])))).

test(parse3) :-
    lex("int main(void) { return 1 + 2; }", Tokens),
    parse(Tokens, Program),
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, block([s(return(binary(add,constant(1),constant(2))))])))).

test(parse5) :-
    lex("int main(void) { int a = 5; int b; b = a - 3; return b; }", Tokens),
    parse(Tokens, Program),
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, block([
        d(declaration(a, constant(5))),
        d(declaration(b, none)),
        s(expression(assignment(var(b), binary(subtract, var(a), constant(3))))),
        s(return(var(b)))
    ])))).

test(parse_if) :-
    lex("int main(void) { if (1) if (2) return 1; else return 2; }", Tokens),
    parse(Tokens, Program),
    assertion(is_ast(Program)),
    assertion(Program = program(function(main, block([
        s(if(constant(1),
            if(constant(2), return(constant(1)), return(constant(2))),
            none))
    ])))).

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

test(exp) :-
    lex("a = b = 5", Tokens),
    once(phrase(exp(Exp), Tokens)),
    assertion(is_exp_ast(Exp)),
    Exp = assignment(var(a), assignment(var(b), constant(5))).

test(exp) :-
    lex("--a + -b++", Tokens),
    once(phrase(exp(Exp), Tokens)),
    assertion(is_exp_ast(Exp)),
    Exp = binary(add, unary(pre_decr, var(a)), unary(negate, unary(post_incr, var(b)))).

test(decl) :-
    lex("int b = 3 + a++;", Tokens),
    once(phrase(declaration(Decl), Tokens)),
    assertion(is_declaration_ast(Decl)),
    Decl = declaration(b, binary(add, constant(3), unary(post_incr, var(a)))).

test(cond) :-
    lex("a ? b ? 1 : 2 : 3", Tokens),
    once(phrase(exp(Exp), Tokens)),
    assertion(is_exp_ast(Exp)),
    Exp = conditional(var(a), conditional(var(b), constant(1), constant(2)), constant(3)).

test(cond_prec) :-
    lex("a || 0 ? 20 : 0", Tokens),
    once(phrase(exp(Exp), Tokens)),
    assertion(is_exp_ast(Exp)),
    Exp = conditional(binary(or, var(a), constant(0)), constant(20), constant(0)).

:- end_tests(parser).
