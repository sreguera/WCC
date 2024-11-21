/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(tacky, [tack/2]).


/** <module> Tacky
 
Tacky generator for Chapter 2 of "Writing a C Compiler".

The generator convertes the AST to Tacky IL.
    * Program = program(FunctionDefinition)
    * FunctionDefinition = function(Name:atom, Body:[instruction])
    * Instruction = 
        | return(val)
        | unary(Unop, src:val, dst:val)
    * Val = 
        | constant(Value:int)
        | var(Name:atom)
    * Unop = '~' | '-'
*/

tack(program(FunDef), program(FunDefTacky)) :-
    reset_gensym,
    tack(FunDef, FunDefTacky).

tack(function(Name, Body), function(Name, Instructions)) :-
    stmt_insts(Body, Instructions).

stmt_insts(return(Exp), Insts) :-
    exp_insts(Exp, Result, Insts, X),
    X = [return(Result)]. 

exp_insts(constant(Int), Dest, T, T) :-
    Dest = constant(Int).
exp_insts(unary(Op, Inner), Dest, Is, T) :-
    exp_insts(Inner, Dest0, Is, X),
    gensym('tmp.', Temp),
    Dest = var(Temp),
    X = [unary(Op, Dest0, Dest)|T].


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(tacky).

test(p0) :-
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

:- end_tests(tacky).

