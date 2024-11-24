/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(tacky, [tack/2]).


/** <module> Tacky
 
Tacky generator for Chapter 3 of "Writing a C Compiler".

The generator convertes the AST to Tacky IL.
    * Program = program(FunctionDefinition)
    * FunctionDefinition = function(Name:atom, Body:[instruction])
    * Instruction = 
        | return(val)
        | unary(Unop, src:val, dst:val)
        | binary(Binop, left:val, right:val, dst:val)
    * Val = 
        | constant(Value:int)
        | var(Name:atom)
    * Unop = complement | negate
    * Binop = add | subtract | multiply | divide | remainder
        | and | or | xor | lshift | rshift
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

:- end_tests(tacky).

