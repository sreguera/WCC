/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(codegen, [generate/2]).

/** <module> Codegen
 
Assembler generator for Chapter 1 of "Writing a C Compiler".

*/

generate(program(FunDef), program(FunDefAsm)) :-
    generate(FunDef, FunDefAsm).
generate(function(Name, Body), function(Name, Instructions)) :-
    generate(Body, Instructions).
generate(return(Exp), [mov(Source, reg), ret]) :-
    generate(Exp, Source).
generate(constant(Int), imm(Int)).

:- begin_tests(codegen).

test(codegen) :-
    generate(program(function(main, return(constant(2)))), Asm),
    Asm = program(function(main, [mov(imm(2), reg), ret])).

:- end_tests(codegen).

