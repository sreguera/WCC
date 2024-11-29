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
    stackify(AsmInsts1, AsmInsts2),
    fixup(AsmInsts2, AsmInsts).

inst_asm(return(Val), [mov(AsmVal, reg(ax)), ret]) :-
    val_asm(Val, AsmVal).
inst_asm(unary(Op, Src, Dst), [mov(AsmSrc, AsmDst), unary(AsmOp, AsmDst)]) :-
    op_asm(Op, AsmOp),
    val_asm(Src, AsmSrc),
    val_asm(Dst, AsmDst).

op_asm(complement, not).
op_asm(negate, neg).

val_asm(constant(Int), imm(Int)).
val_asm(var(Ident), pseudo(Ident)).


%-------------------%
%   STACKIFY PASS   %
%-------------------%

stackify(Insts0, Insts) :-
    empty_assoc(M0),
    replace(Insts0, state(0, M0), Insts1, state(Pos, _)),
    Size is - Pos,
    Insts = [allocate_stack(Size) | Insts1].

%!  replace(+InstsIn, +StateIn, -InstsOut, -StateOut)

replace([], S, [], S).
replace([I0|I0s], S0, [I|Is], S) :-
    rep_inst(I0, S0, I, S1),
    replace(I0s, S1, Is, S).

%!  rep_inst(+InstIn, +StateIn, -InstOut, -StateOut)

rep_inst(ret, S, ret, S).
rep_inst(unary(Op, Val0), S0, unary(Op, Val), S) :-
    rep_val(Val0, S0, Val, S).
rep_inst(mov(Val0, Val1), S0, mov(Val2, Val3), S) :-
    rep_val(Val0, S0, Val2, S1),
    rep_val(Val1, S1, Val3, S).

%!  rep_val(+ValIn, +StateIn, -ValOut, -StateOut)

rep_val(imm(Int), S, imm(Int), S).
rep_val(reg(R), S, reg(R), S).
rep_val(pseudo(Id), state(P0, M0), stack(PId), state(P, M)) :-
    (   get_assoc(Id, M0, PId)
    ->  P = P0,
        M = M0
    ;   PId is P0 - 4,
        P = PId,
        put_assoc(Id, M0, PId, M)
    ).

%----------------%
%   FIXUP PASS   %
%----------------%

fixup(Insts0, Insts) :-
    maplist(rep_reg, Insts0, Insts1),
    flatten(Insts1, Insts).

% Can't have two memory arguments
rep_reg(mov(stack(X), stack(Y)), Insts) :- !,
    Insts = [
        mov(stack(X), reg(r10)),
        mov(reg(r10), stack(Y))
    ].
% Any other thing goes unchanged
rep_reg(I, I).


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(codegen).

test(codegen) :-
    generate(program(function(main, [
        unary(negate, constant(2), var('tmp.1')),
        unary(complement, var('tmp.1'), var('tmp.2')),
        return(var('tmp.2'))
    ])), Asm),
    Asm = program(function(main, [
        allocate_stack(8),
        mov(imm(2), stack(-4)),
        unary(neg, stack(-4)),
        mov(stack(-4), reg(r10)),
        mov(reg(r10), stack(-8)),
        unary(not, stack(-8)),
        mov(stack(-8), reg(ax)),
        ret
    ])).

:- end_tests(codegen).

