/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(tacky, [tack/2]).

/** <module> Tacky
 
Tacky generator for Chapter 4 of "Writing a C Compiler".

The generator convertes the AST to Tacky IL.
    * Program = program(FunctionDefinition)
    * FunctionDefinition = function(Name:atom, Body:[instruction])
    * Instruction = 
        | return(val)
        | unary(Unop, src:val, dst:val)
        | binary(Binop, left:val, right:val, dst:val)
        | copy(src:val, dst:val)
        | jump(identifier)
        | jump_if_zero(condition, identifier)
        | jump_if_not_zero(condition, identifier)
        | label(identifier)
    * Val = 
        | constant(Value:int)
        | var(Name:atom)
    * Unop = complement | negate | not
    * Binop = add | subtract | multiply | divide | remainder
        | bit_and | bit_or | bit_xor | lshift | rshift
        | equal | not_equal
        | less_than | less_eq | greater_than | greater_eq
*/

tack(program(FunDef), program(FunDefTacky)) :-
    reset_gensym,
    tack(FunDef, FunDefTacky).

tack(function(Name, Body), function(Name, Instructions)) :-
    stmt_insts(Body, Instructions).

stmt_insts(return(Exp), Insts) :-
    exp_insts(Exp, Result, Insts, X),
    X = [return(Result)]. %TODO??? see Xs below

exp_insts(constant(Int), Dest, T, T) :-
    Dest = constant(Int).
exp_insts(unary(Op, Inner), Dest, Is, T) :-
    exp_insts(Inner, Dest0, Is, X),
    gensym('tmp.', Temp),
    Dest = var(Temp),
    X = [unary(Op, Dest0, Dest)|T].
exp_insts(binary(Op, Left, Right), Dest, Is, T) :-
    binary_insts(Op, Left, Right, Dest, Is, T).

binary_insts(and, Left, Right, Dest, Is, T) :- !, %TODO replace ! with index on op
    exp_insts(Left, DestL, Is, I1),
    I1 = [jump_if_zero(DestL, False)|I2],
    exp_insts(Right, DestR, I2, I3),
    I3 = [jump_if_zero(DestR, False)|I4],
    I4 = [
        copy(constant(1), Dest),
        jump(End),
        label(False),
        copy(constant(0), Dest),
        label(End)
        | T
    ],
    gensym('tmp.', Temp),
    Dest = var(Temp),
    gensym('and_end', End),
    gensym('and_false', False).
binary_insts(or, Left, Right, Dest, Is, T) :- !,
    exp_insts(Left, DestL, Is, I1),
    I1 = [jump_if_not_zero(DestL, True)|I2],
    exp_insts(Right, DestR, I2, I3),
    I3 = [jump_if_not_zero(DestR, True)|I4],
    I4 = [
        copy(constant(0), Dest),
        jump(End),
        label(True),
        copy(constant(1), Dest),
        label(End)
        | T
    ],
    gensym('tmp.', Temp),
    Dest = var(Temp),
    gensym('or_end', End),
    gensym('or_true', True).
binary_insts(Op, Left, Right, Dest, Is, T) :-
    Op \= and,
    Op \= or,
    exp_insts(Left, DestL, Is, I1),
    exp_insts(Right, DestR, I1, I2),
    gensym('tmp.', Temp),
    Dest = var(Temp),
    I2 = [binary(Op, DestL, DestR, Dest)|T].


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(tacky).

test(p1) :-
    tack(program(function(main, return(constant(2)))), Tacky),
    Tacky = program(function(main, [return(constant(2))])).

test(p2) :-
    tack(program(function(main, return(unary(negate, unary(complement, unary(negate, constant(2))))))), Tacky),
    Tacky = program(function(main, [
        unary(negate, constant(2), var('tmp.1')),
        unary(complement, var('tmp.1'), var('tmp.2')),
        unary(negate, var('tmp.2'), var('tmp.3')),
        return(var('tmp.3'))
    ])).

test(p3a) :-
    tack(program(function(main,return(binary(add,constant(1),constant(2))))), Tacky),
    Tacky = program(function(main, [
        binary(add, constant(1), constant(2), var('tmp.1')),
        return(var('tmp.1'))
    ])).

test(p3) :-
    tack(program(function(main, return(
        binary(add, constant(2), binary(multiply, constant(3), constant(4)))))), Tacky),
    Tacky = program(function(main, [
        binary(multiply, constant(3), constant(4), var('tmp.1')),
        binary(add, constant(2), var('tmp.1'), var('tmp.2')),
        return(var('tmp.2'))
    ])).

test(and) :-
    reset_gensym,
    exp_insts(binary(and, constant(8), constant(0)), _Dest, Is, []),
    Is = [
        jump_if_zero(constant(8), and_false1),
        jump_if_zero(constant(0), and_false1),
        copy(constant(1), var('tmp.1')),
        jump(and_end1),
        label(and_false1),
        copy(constant(0), var('tmp.1')),
        label(and_end1)
    ].

test(or) :-
    reset_gensym,
    exp_insts(binary(or, constant(8), constant(0)), _Dest, Is, []),
    Is = [
        jump_if_not_zero(constant(8), or_true1),
        jump_if_not_zero(constant(0), or_true1),
        copy(constant(0), var('tmp.1')),
        jump(or_end1),
        label(or_true1),
        copy(constant(1), var('tmp.1')),
        label(or_end1)
    ].

:- end_tests(tacky).

