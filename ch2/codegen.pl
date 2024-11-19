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
    flatten(AsmInsts0, AsmInsts1),
    replace(AsmInsts1, AsmInsts2, Pos),
    maplist(rep_reg, AsmInsts2, AsmInsts3),
    flatten(AsmInsts3, AsmInsts4),
    Size is - Pos,
    AsmInsts = [allocate_stack(Size) | AsmInsts4].

inst_asm(return(Val), [mov(AsmVal, reg(ax)), ret]) :-
    val_asm(Val, AsmVal).
inst_asm(unary(Op, Src, Dst), [mov(AsmSrc, AsmDst), unary(Op, AsmDst)]) :-
    val_asm(Src, AsmSrc),
    val_asm(Dst, AsmDst).

val_asm(constant(Int), imm(Int)).
val_asm(var(Ident), pseudo(Ident)).


%! replace(+InstsIn, -InstsOut, -Pos)

replace(I0s, Is, P) :-
    empty_assoc(M0),
    replace(I0s, 0, M0, Is, P, _).

replace([], P, M, [], P, M).
replace([I0|I0s], P0, M0, [I|Is], P, M) :-
    rep_inst(I0, P0, M0, I, P1, M1),
    replace(I0s, P1, M1, Is, P, M).

%!  rep_inst(+InstIn, +PosIn, +MapIn, -InstOut, -PosOut, -MapOut)

rep_inst(ret, P, M, ret, P, M).
rep_inst(unary(Op, Val0), P0, M0, unary(Op, Val), P, M) :-
    rep_val(Val0, P0, M0, Val, P, M).
rep_inst(mov(Val0, Val1), P0, M0, mov(Val2, Val3), P, M) :-
    rep_val(Val0, P0, M0, Val2, P1, M1),
    rep_val(Val1, P1, M1, Val3, P, M).

%!  rep_val(+ValIn, +PosIn, +MapIn, -ValOut, -PosOut, -MapOut)

rep_val(imm(Int), P, M, imm(Int), P, M).
rep_val(reg(R), P, M, reg(R), P, M).
rep_val(pseudo(Id), P0, M0, stack(PId), P, M) :-
    (   get_assoc(Id, M0, PId)
    ->  P = P0,
        M = M0
    ;   PId is P0 - 4,
        P = PId,
        put_assoc(Id, M0, PId, M)
    ).


rep_reg(mov(stack(X), stack(Y)), [mov(stack(X), reg(r10)), mov(reg(r10), stack(Y))]) :- !.
rep_reg(I, I).


:- begin_tests(codegen).

test(codegen) :-
    generate(program(function(main, [
        unary('-', constant(2), var('tmp.1')),
        unary('~', var('tmp.1'), var('tmp.2')),
        return(var('tmp.2'))
    ])), Asm),
    Asm = program(function(main, [
        allocate_stack(8),
        mov(imm(2), stack(-4)),
        unary('-', stack(-4)),
        mov(stack(-4), reg(r10)),
        mov(reg(r10), stack(-8)),
        unary('~', stack(-8)),
        mov(stack(-8), reg(ax)),
        ret
    ])).

:- end_tests(codegen).

