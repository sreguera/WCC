/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(emitter, [emit/2]).
:- use_module(codegen, [is_assembly/1]).

/** <module> Emitter
 
Assembler emitter for Chapter 1 of "Writing a C Compiler".

*/

emit(Asm, Output) :-
    assertion(is_assembly(Asm)),
    with_output_to(string(Output), emit(Asm)).
   
emit(program(FunDef)) :-
    emit(FunDef),
    format("~4|.section .note.GNU-stack,\"\",@progbits~n").
emit(function(Name, Instructions)) :-
    format("~4|.globl ~s~n", [Name]),
    format("~s:~n", [Name]),
    maplist(emit, Instructions).
emit(mov(Source, Dest)) :-
    exp_emit(Source, SOut),
    exp_emit(Dest, EOut),
    format("~4|movl ~12|~s, ~s~n", [SOut, EOut]).
emit(ret) :-
    format("~4|ret~n").

exp_emit(reg, "%eax").
exp_emit(imm(Int), Out) :-
    format(string(Out), "$~d", [Int]).


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(emit).

test(emit) :-
    emit(program(function(main, [mov(imm(2), reg), ret])), Asm),
    assertion(Asm = "    .globl main
main:
    movl    $2, %eax
    ret
    .section .note.GNU-stack,\"\",@progbits
").

:- end_tests(emit).

