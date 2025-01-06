/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(codegen, [generate/2]).
:- use_module(tacky, [is_tacky/1]).

/** <module> Codegen
 
Assembler generator for Chapter 8 of "Writing a C Compiler".

*/

generate(program(FunDef), program(FunDefAsm)) :-
    assertion(is_tacky(program(FunDef))),
    generate(FunDef, FunDefAsm).

generate(function(Name, Insts), function(Name, AsmInsts)) :-
    maplist(inst_asm, Insts, AsmInsts0),
    flatten(AsmInsts0, AsmInsts1),
    stackify(AsmInsts1, AsmInsts2),
    fixup(AsmInsts2, AsmInsts).

inst_asm(return(Val), [mov(AsmVal, reg(ax)), ret]) :-
    val_asm(Val, AsmVal).
inst_asm(unary(Op, Src, Dst), AsmInsts) :-
    unary_asm(Op, Src, Dst, AsmInsts).
inst_asm(binary(Op, Left, Right, Dst), AsmInsts) :-
    binary_asm(Op, Left, Right, Dst, AsmInsts).
inst_asm(jump(Target), [jmp(Target)]).
inst_asm(jump_if_zero(Condition, Target), AsmInsts) :-
    val_asm(Condition, AsmCondition),
    AsmInsts = [
        cmp(imm(0), AsmCondition),
        jmp_cc(e, Target)
    ].
inst_asm(jump_if_not_zero(Condition, Target), AsmInsts) :-
    val_asm(Condition, AsmCondition),
    AsmInsts = [
        cmp(imm(0), AsmCondition),
        jmp_cc(ne, Target)
    ].
inst_asm(copy(Src, Dst), [mov(AsmSrc, AsmDst)]) :-
    val_asm(Src, AsmSrc),
    val_asm(Dst, AsmDst).
inst_asm(label(Ident), [label(Ident)]).

unary_asm(Op, Src, Dst, AsmInsts) :-
    val_asm(Src, AsmSrc),
    val_asm(Dst, AsmDst),
    (   Op = not
    ->  AsmInsts = [
            cmp(imm(0), AsmSrc),
            mov(imm(0), AsmDst),
            set_cc(e, AsmDst)
        ]
    ;   op_asm(Op, AsmOp),
        AsmInsts = [
            mov(AsmSrc, AsmDst),
            unary(AsmOp, AsmDst)
        ]
    ).

binary_asm(Op, Left, Right, Dst, AsmInsts) :-
    val_asm(Left, AsmLeft),
    val_asm(Right, AsmRight),
    val_asm(Dst, AsmDst),
    (   Op = divide
    ->  AsmInsts = [
            mov(AsmLeft, reg(ax)),
            cdq,
            idiv(AsmRight),
            mov(reg(ax), AsmDst)
        ]
    ;   Op = remainder
    ->  AsmInsts = [
            mov(AsmLeft, reg(ax)),
            cdq,
            idiv(AsmRight),
            mov(reg(dx), AsmDst)
        ]
    ;   rel_op(Op, RelOp)
    ->  AsmInsts = [
            cmp(AsmRight, AsmLeft),
            mov(imm(0), AsmDst),
            set_cc(RelOp, AsmDst)
        ]
    ;   op_asm(Op, AsmOp),
        AsmInsts = [
            mov(AsmLeft, AsmDst),
            binary(AsmOp, AsmRight, AsmDst)
        ]
    ).

rel_op(less_than,    l).
rel_op(less_eq,      le).
rel_op(greater_than, g).
rel_op(greater_eq,   ge).
rel_op(equal,        e).
rel_op(not_equal,    ne).

op_asm(complement, not).
op_asm(negate, neg).
op_asm(add, add).
op_asm(subtract, sub).
op_asm(multiply, mult).
op_asm(bit_and, and).
op_asm(bit_or, or).
op_asm(bit_xor, xor).
op_asm(lshift, sal).
op_asm(rshift, sar).

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
rep_inst(cdq, S, cdq, S).
rep_inst(jmp(T), S, jmp(T), S).
rep_inst(label(T), S, label(T), S).
rep_inst(jmp_cc(Op, T), S, jmp_cc(Op, T), S).
rep_inst(set_cc(Op, Val0), S0, set_cc(Op, Val), S) :-
    rep_val(Val0, S0, Val, S).
rep_inst(cmp(Val0, Val1), S0, cmp(Val2, Val3), S) :-
    rep_val(Val0, S0, Val2, S1),
    rep_val(Val1, S1, Val3, S).
rep_inst(idiv(Val0), S0, idiv(Val), S) :-
    rep_val(Val0, S0, Val, S).
rep_inst(unary(Op, Val0), S0, unary(Op, Val), S) :-
    rep_val(Val0, S0, Val, S).
rep_inst(binary(Op, Val0, Val1), S0, binary(Op, Val2, Val3), S) :-
    rep_val(Val0, S0, Val2, S1),
    rep_val(Val1, S1, Val3, S).
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
rep_reg(binary(add, stack(X), stack(Y)), Insts) :- !,
    rep_src_by_r10(add, stack(X), stack(Y), Insts).
rep_reg(binary(sub, stack(X), stack(Y)), Insts) :- !,
    rep_src_by_r10(sub, stack(X), stack(Y), Insts).
rep_reg(binary(and, stack(X), stack(Y)), Insts) :- !,
    rep_src_by_r10(and, stack(X), stack(Y), Insts).
rep_reg(binary(or, stack(X), stack(Y)), Insts) :- !,
    rep_src_by_r10(or, stack(X), stack(Y), Insts).
rep_reg(binary(xor, stack(X), stack(Y)), Insts) :- !,
    rep_src_by_r10(xor, stack(X), stack(Y), Insts).
% count must be imm or cl. Use the reg and don't worry.
rep_reg(binary(sal, X, Y), Insts) :- !,
    Insts = [
        mov(X, reg(cx)),
        binary(sal, reg(cx), Y)
    ].
rep_reg(binary(sar, X, Y), Insts) :- !,
    Insts = [
        mov(X, reg(cx)),
        binary(sar, reg(cx), Y)
    ].
% mult can't use memory as destination
rep_reg(binary(mult, X, stack(Y)), Insts) :- !,
    Insts = [
        mov(stack(Y), reg(r11)),
        binary(mult, X, reg(r11)),
        mov(reg(r11), stack(Y))
    ].
% cmp can't have two memory ops or an imm target
rep_reg(cmp(stack(X), stack(Y)), Insts) :- !,
    Insts = [
        mov(stack(X), reg(r10)),
        cmp(reg(r10), stack(Y))
    ].
rep_reg(cmp(X, imm(Y)), Insts) :- !,
    Insts = [
        mov(imm(Y), reg(r11)),
        cmp(X, reg(r11))
    ].
% idiv can't operate on a constant
rep_reg(idiv(imm(V)), Insts) :- !,
    Insts = [
        mov(imm(V), reg(r10)),
        idiv(reg(r10))
    ].
% Any other thing goes unchanged
rep_reg(I, I).

rep_src_by_r10(Op, X, Y, Insts) :-
    Insts = [
        mov(X, reg(r10)),
        binary(Op, reg(r10), Y)
    ].


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

test(codegen) :-
    generate(program(function(main, [
        binary(add, constant(1), constant(2), var('tmp.1')),
        return(var('tmp.1'))
    ])), Asm),
    Asm = program(function(main, [
        allocate_stack(4),
        mov(imm(1), stack(-4)),
        binary(add, imm(2), stack(-4)),
        mov(stack(-4), reg(ax)),
        ret
    ])).

test(codegen_bitwise) :-
    Program = program(function(main, [
        binary(less_than, constant(7), constant(5), var('tmp.1')),
        binary(bit_xor, constant(5), var('tmp.1'), var('tmp.2')),
        return(var('tmp.2'))
    ])),
    generate(Program, Asm),
    Asm = program(function(main, [
        allocate_stack(8),
        mov(imm(7), reg(r11)),
        cmp(imm(5), reg(r11)),
        mov(imm(0), stack(-4)),
        set_cc(l, stack(-4)),
        mov(imm(5), stack(-8)),
        mov(stack(-4), reg(r10)),
        binary(xor, reg(r10), stack(-8)),
        mov(stack(-8), reg(ax)),
        ret
    ])).

test(fixup) :-
    fixup([binary(mult, imm(3), stack(-4))], Insts),
    Insts = [
        mov(stack(-4),reg(r11)),
        binary(mult,imm(3),reg(r11)),
        mov(reg(r11),stack(-4))
    ].

test(label) :-
    inst_asm(label(xyz), Is),
    Is = [label(xyz)].

test(copy) :-
    inst_asm(copy(constant(1), var('tmp.1')), Is),
    Is = [mov(imm(1), pseudo('tmp.1'))].

test(jump) :-
    inst_asm(jump(xyz), Is),
    Is = [jmp(xyz)].

test(jump_if_zero) :-
    inst_asm(jump_if_zero(constant(1), abc), Is),
    Is = [cmp(imm(0), imm(1)), jmp_cc(e, abc)].

test(not) :-
    inst_asm(unary(not, constant(1), var('tmp.1')), Is),
    Is = [cmp(imm(0), imm(1)), mov(imm(0), pseudo('tmp.1')), set_cc(e, pseudo('tmp.1'))].

test(rel) :-
    inst_asm(binary(greater_eq, constant(1), constant(2), var('tmp.1')), Is),
    Is = [cmp(imm(2), imm(1)), mov(imm(0), pseudo('tmp.1')), set_cc(ge, pseudo('tmp.1'))].

:- end_tests(codegen).

