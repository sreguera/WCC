/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(codegen,
    [ is_assembly/1,    % Succeeds if Program is a valid assembly program.
      generate/2        % Generate the Assembly for the given Ast.
    ]).
:- use_module(parser, [is_ast/1]).

/** <module> Codegen
 
Assembler generator for Chapter 1 of "Writing a C Compiler".

*/

%!  is_assembly(+Program)
%
%   Succeeds if Program is a valid assembly program.

is_assembly(Program) :-
    is_assembly_program(Program).

is_assembly_program(program(FunDef)) :-
    is_assembly_function(FunDef).

is_assembly_function(function(Name, Body)) :-
    atom(Name),
    forall(member(Inst, Body), is_assembly_instruction(Inst)).

is_assembly_instruction(mov(Src, Dst)) :-
    is_assembly_operand(Src),
    is_assembly_operand(Dst).
is_assembly_instruction(ret).

is_assembly_operand(imm(Int)) :-
    integer(Int).
is_assembly_operand(reg).

%!  generate(+Ast, -Assembly)
%
%   Generate the Assembly for the given Ast.

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
    assertion(is_assembly(Asm)),
    assertion(Asm = program(function(main, [mov(imm(2), reg), ret]))).

:- end_tests(codegen).

