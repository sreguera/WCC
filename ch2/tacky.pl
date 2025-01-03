/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(tacky, [tack/2]).
:- use_module(parser, [is_ast/1]).


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
    * Unop = complement | negate
*/

tack(Program0, Program) :-
    assertion(is_ast(Program0)),
    tack_program(Program0, Program).

tack_program(program(FunDef), program(FunDefTacky)) :-
    reset_gensym,
    tack_function(FunDef, FunDefTacky).

tack_function(function(Name, Body), function(Name, Instructions)) :-
    stmt_insts(Body, Instructions).

stmt_insts(return(Exp), I0) :-
    exp_insts(Exp, Result, I0, I1),
    I1 = [return(Result)]. 

exp_insts(constant(Int), constant(Int), Is, Is).
exp_insts(unary(Op, Inner), Dest, I0, Is) :-
    exp_insts(Inner, Dest0, I0, I1),
    mk_tmpvar(Dest),
    I1 = [unary(Op, Dest0, Dest)|Is].

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

:- end_tests(tacky).

