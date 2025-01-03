/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(codegen, [generate/2]).
:- use_module(parser, [is_ast/1]).


/** <module> Codegen
 
Assembler generator for Chapter 1 of "Writing a C Compiler".

*/

generate(Program0, Program) :-
    assertion(is_ast(Program0)),
    generate_program(Program0, Program).

generate_program(program(FunDef), program(FunDefAsm)) :-
    generate_function(FunDef, FunDefAsm).
generate_function(function(Name, Body), function(Name, Instructions)) :-
    generate_statement(Body, Instructions).
generate_statement(return(Exp), [mov(Source, reg), ret]) :-
    generate_exp(Exp, Source).
generate_exp(constant(Int), imm(Int)).


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(codegen).

test(codegen) :-
    generate(program(function(main, return(constant(2)))), Asm),
    Asm = program(function(main, [mov(imm(2), reg), ret])).

:- end_tests(codegen).

