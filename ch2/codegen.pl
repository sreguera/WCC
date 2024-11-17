/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(codegen, [generate/2]).

/** <module> Codegen
 
Assembler generator for Chapter 2 of "Writing a C Compiler".

*/

generate(program(FunDef), program(FunDefAsm)) :-
    generate(FunDef, FunDefAsm).

generate(function(Name, Insts), function(Name, AsmInsts)) :-
    maplist(inst_asm, Insts, AsmInsts0),
    flatten(AsmInsts0, AsmInsts).    

inst_asm(return(Val), [mov(AsmVal, reg(ax)), ret]) :-
    val_asm(Val, AsmVal).
inst_asm(unary(Op, Src, Dst), [mov(AsmSrc, AsmDst), unary(Op, AsmDst)]) :-
    val_asm(Src, AsmSrc),
    val_asm(Dst, AsmDst).

val_asm(constant(Int), imm(Int)).
val_asm(var(Ident), pseudo(Ident)).

:- begin_tests(codegen).

test(codegen) :-
    generate(program(function(main, [
        unary('-', constant(2), var('tmp.1')),
        unary('~', var('tmp.1'), var('tmp.2')),
        return(var('tmp.2'))
    ])), Asm),
    Asm =  program(function(main, [
        mov(imm(2), pseudo('tmp.1')),
        unary('-', pseudo('tmp.1')),
        mov(pseudo('tmp.1'), pseudo('tmp.2')),
        unary('~', pseudo('tmp.2')),
        mov(pseudo('tmp.2'), reg(ax)),
        ret
    ])).

:- end_tests(codegen).

