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

stmt_insts(return(Exp), I0) :-
    exp_insts(Exp, Result, I0, I1),
    I1 = [return(Result)].

exp_insts(constant(Int), constant(Int), Is, Is).
exp_insts(unary(Op, Inner), Dest, I0, Is) :-
    exp_insts(Inner, Dest0, I0, I1),
    mk_tmpvar(Dest),
    I1 = [unary(Op, Dest0, Dest)|Is].
exp_insts(binary(Op, Left, Right), Dest, I0, Is) :-
    binary_insts(Op, Left, Right, Dest, I0, Is).

binary_insts(and, Left, Right, Dest, I0, Is) :- !, %TODO replace ! with index on op
    exp_insts(Left, DestL, I0, I1),
    I1 = [jump_if_zero(DestL, FalseLabel)|I2],
    exp_insts(Right, DestR, I2, I3),
    I3 = [
        jump_if_zero(DestR, FalseLabel),
        copy(constant(1), Dest),
        jump(EndLabel),
        label(FalseLabel),
        copy(constant(0), Dest),
        label(EndLabel)
        | Is
    ],
    mk_tmpvar(Dest),
    gensym('and_end', EndLabel),
    gensym('and_false', FalseLabel).
binary_insts(or, Left, Right, Dest, I0, Is) :- !,
    exp_insts(Left, DestL, I0, I1),
    I1 = [jump_if_not_zero(DestL, TrueLabel)|I2],
    exp_insts(Right, DestR, I2, I3),
    I3 = [
        jump_if_not_zero(DestR, TrueLabel),
        copy(constant(0), Dest),
        jump(EndLabel),
        label(TrueLabel),
        copy(constant(1), Dest),
        label(EndLabel)
        | Is
    ],
    mk_tmpvar(Dest),
    gensym('or_end', EndLabel),
    gensym('or_true', TrueLabel).
binary_insts(Op, Left, Right, Dest, I0, Is) :-
    Op \= and,
    Op \= or,
    exp_insts(Left, DestL, I0, I1),
    exp_insts(Right, DestR, I1, I2),
    mk_tmpvar(Dest),
    I2 = [binary(Op, DestL, DestR, Dest)|Is].

mk_tmpvar(var(UniqueName)) :-
    gensym('tmp.', UniqueName).


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

