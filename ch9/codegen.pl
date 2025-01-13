/* Copyright 2025 José Sebastián Reguera Candal
*/
:- module(codegen,
    [ is_assembly/1,    % Succeeds if Program is a valid assembly program.
      generate/2        % Generate the Assembly for the given Ast.
    ]).
:- use_module(tacky, [is_tacky/1]).

/** <module> Codegen
 
Assembler generator for Chapter 9 of "Writing a C Compiler".

*/

%!  is_assembly(+Program)
%
%   Succeeds if Program is a valid assembly program.

is_assembly(Program) :-
    is_assembly_program(Program).

is_assembly_program(program(FunDefs)) :-
    forall(member(FunDef, FunDefs), is_assembly_function(FunDef)).

is_assembly_function(function(Name, Insts)) :-
    atom(Name),
    forall(member(Inst, Insts), is_assembly_instruction(Inst)).

is_assembly_instruction(mov(Src, Dst)) :-
    is_assembly_operand(Src),
    is_assembly_operand(Dst).
is_assembly_instruction(binary(Op, Src, Dst)) :-
    is_assembly_binary_operator(Op),
    is_assembly_operand(Src),
    is_assembly_operand(Dst).
is_assembly_instruction(unary(Op, Dst)) :-
    is_assembly_unary_operator(Op),
    is_assembly_operand(Dst).
is_assembly_instruction(cmp(Op1, Op2)) :-
    is_assembly_operand(Op1),
    is_assembly_operand(Op2).
is_assembly_instruction(idiv(Dst)) :-
    is_assembly_operand(Dst).
is_assembly_instruction(cdq).
is_assembly_instruction(jmp(Ident)) :-
    atom(Ident).
is_assembly_instruction(jmp_cc(Cond, Ident)) :-
    is_assembly_condition(Cond),
    atom(Ident).
is_assembly_instruction(set_cc(Cond, Dst)) :-
    is_assembly_condition(Cond),
    is_assembly_operand(Dst).
is_assembly_instruction(label(Ident)) :-
    atom(Ident).
is_assembly_instruction(allocate_stack(Int)) :-
    integer(Int).
is_assembly_instruction(deallocate_stack(Int)) :-
    integer(Int).
is_assembly_instruction(push(Operand)) :-
    is_assembly_operand(Operand).
is_assembly_instruction(call(Ident)) :-
    atom(Ident).
is_assembly_instruction(ret).

is_assembly_unary_operator(neg).
is_assembly_unary_operator(not).

is_assembly_binary_operator(add).
is_assembly_binary_operator(sub).
is_assembly_binary_operator(mult).
is_assembly_binary_operator(and).
is_assembly_binary_operator(or).
is_assembly_binary_operator(xor).
is_assembly_binary_operator(sal).
is_assembly_binary_operator(sar).

is_assembly_operand(imm(Int)) :-
    integer(Int).
is_assembly_operand(reg(Reg)) :-
    is_assembly_reg(Reg).
is_assembly_operand(pseudo(Id)) :-
    atom(Id).
is_assembly_operand(stack(Int)) :-
    integer(Int).

is_assembly_condition(e).
is_assembly_condition(ne).
is_assembly_condition(g).
is_assembly_condition(ge).
is_assembly_condition(l).
is_assembly_condition(le).

is_assembly_reg(ax).
is_assembly_reg(cx).
is_assembly_reg(dx).
is_assembly_reg(di).
is_assembly_reg(si).
is_assembly_reg(r8).
is_assembly_reg(r9).
is_assembly_reg(r10).
is_assembly_reg(r11).

%!  generate(+Ast, -Assembly)
%
%   Generate the Assembly for the given Ast.

generate(program(FunDef), program(FunDefAsm)) :-
    assertion(is_tacky(program(FunDef))),
    maplist(generate_function, FunDef, FunDefAsm).

generate_function(function(Name, Args, Insts), function(Name, AsmInsts)) :-
    callee_copy_args(Args, ArgIs),
    maplist(inst_asm, Insts, AsmInsts0),
    flatten([ArgIs, AsmInsts0], AsmInsts1),
    stackify(AsmInsts1, AsmInsts2),
    fixup(AsmInsts2, AsmInsts).

callee_copy_args(Args, [RIs, MIs]) :-
    callee_copy_reg_args(Args, RestArgs, RIs),
    callee_copy_mem_args(RestArgs, MIs).

callee_copy_reg_args(Args, Rest, Insts) :-
    callee_copy_reg_args([di, si, dx, cx, r8, r9], Args, Rest, Insts).

callee_copy_reg_args([Reg|Regs], [Arg|Args], Rest, [I0|Is]) :-
    !,
    I0 = mov(reg(Reg), pseudo(Arg)),
    callee_copy_reg_args(Regs, Args, Rest, Is).
callee_copy_reg_args([], Args, Args, []) :- !.
callee_copy_reg_args(_, [], [], []).

callee_copy_mem_args(Args, Is) :-
    callee_copy_mem_args(Args, 16, Is).

callee_copy_mem_args([], _, []).
callee_copy_mem_args([Arg|Args], Idx0, [I0|Is]) :-
    I0 = mov(stack(Idx0), pseudo(Arg)),
    Idx is Idx0 + 8,
    callee_copy_mem_args(Args, Idx, Is).

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
inst_asm(funcall(Id, Args, Dst), AsmInsts) :- % XXX
    length(Args, NArgs),
    Pad is ((NArgs - 6) mod 2) * 8,
    (   Pad = 0
    ->  I0 = I1
    ;   I0 = [allocate_stack(Pad)|I1]
    ),
    caller_copy_args(Args, I1, I2),
    I2 = [call(Id)|I3],
    val_asm(Dst, AsmDst),
    ToRemove is 8 * max((NArgs - 6), 0) + Pad,
    (   ToRemove = 0
    ->  I3 = I4
    ;   I3 = [deallocate_stack(ToRemove)|I4]
    ),
    I4 = [mov(reg(ax), AsmDst)],
    AsmInsts = I0. 

caller_copy_args(Args, I0, Is) :-
    caller_copy_reg_args(Args, RestArgs, I0, I1),
    caller_copy_mem_args(RestArgs, I1, Is).

caller_copy_reg_args(Args, Rest, I0, Is) :-
    caller_copy_reg_args([di, si, dx, cx, r8, r9], Args, Rest, I0, Is).

caller_copy_reg_args([Reg|Regs], [Arg|Args], Rest, I0, Is) :-
    !,
    val_asm(Arg, AsmVal),
    I0 = [mov(AsmVal, reg(Reg))|I1],
    caller_copy_reg_args(Regs, Args, Rest, I1, Is).
caller_copy_reg_args([], Args, Args, Is, Is) :- !.
caller_copy_reg_args(_, [], [], Is, Is).

caller_copy_mem_args([], Is, Is).
caller_copy_mem_args([Arg|Args], I0, Is) :-
    val_asm(Arg, AsmVal),
    (   AsmVal = imm(_)
    ->  I0 = [push(AsmVal)|I1]
    ;   AsmVal = reg(_)
    ->  I0 = [push(AsmVal)|I1]
    ;   I0 = [mov(AsmVal, reg(ax)), push(reg(ax))|I1]
    ),
    caller_copy_mem_args(Args, I1, Is).

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
    AlignedSize is (Size \/ 15) + 1, % Align to the next multiple of 16
    Insts = [allocate_stack(AlignedSize) | Insts1].



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
rep_inst(allocate_stack(N), S, allocate_stack(N), S).
rep_inst(deallocate_stack(N), S, deallocate_stack(N), S).
rep_inst(call(Id), S, call(Id), S).
rep_inst(push(Val0), S0, push(Val), S) :-
    rep_val(Val0, S0, Val, S).

%!  rep_val(+ValIn, +StateIn, -ValOut, -StateOut)

rep_val(imm(Int), S, imm(Int), S).
rep_val(reg(R), S, reg(R), S).
rep_val(stack(Int), S, stack(Int), S).
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
    generate(program([function(main, [], [
        unary(negate, constant(2), var('tmp.1')),
        unary(complement, var('tmp.1'), var('tmp.2')),
        return(var('tmp.2'))
    ])]), Asm),
    assertion(is_assembly(Asm)),
    assertion(Asm = program([function(main, [
        allocate_stack(16),
        mov(imm(2), stack(-4)),
        unary(neg, stack(-4)),
        mov(stack(-4), reg(r10)),
        mov(reg(r10), stack(-8)),
        unary(not, stack(-8)),
        mov(stack(-8), reg(ax)),
        ret
    ])])).

test(codegen) :-
    generate(program([function(main, [], [
        binary(add, constant(1), constant(2), var('tmp.1')),
        return(var('tmp.1'))
    ])]), Asm),
    assertion(is_assembly(Asm)),
    assertion(Asm = program([function(main, [
        allocate_stack(16),
        mov(imm(1), stack(-4)),
        binary(add, imm(2), stack(-4)),
        mov(stack(-4), reg(ax)),
        ret
    ])])).

test(codegen_bitwise) :-
    Program = program([function(main, [], [
        binary(less_than, constant(7), constant(5), var('tmp.1')),
        binary(bit_xor, constant(5), var('tmp.1'), var('tmp.2')),
        return(var('tmp.2'))
    ])]),
    generate(Program, Asm),
    assertion(is_assembly(Asm)),
    assertion(Asm = program([function(main, [
        allocate_stack(16),
        mov(imm(7), reg(r11)),
        cmp(imm(5), reg(r11)),
        mov(imm(0), stack(-4)),
        set_cc(l, stack(-4)),
        mov(imm(5), stack(-8)),
        mov(stack(-4), reg(r10)),
        binary(xor, reg(r10), stack(-8)),
        mov(stack(-8), reg(ax)),
        ret
    ])])).

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

test(codegen_function) :-
    Program = program([
        function(main, [], [
            funcall(foo, [constant(2), constant(1), constant(0)], var('tmp.1')),
            return(var('tmp.1')),
            return(constant(0))
        ]),
        function(foo, ['var.x.3', 'var.y.4'], [
            binary(subtract, var('var.x.3'), var('var.y.4'), var('tmp.2')),
            return(var('tmp.2')),
            return(constant(0))
        ])
    ]),
    generate(Program, Asm),
    assertion(is_assembly(Asm)),
    assertion(Asm = program([
        function(main, [
            allocate_stack(16),
            allocate_stack(8),
            mov(imm(2), reg(di)),
            mov(imm(1), reg(si)),
            mov(imm(0), reg(dx)),
            call(foo),
            deallocate_stack(8),
            mov(reg(ax), stack(-4)),
            mov(stack(-4), reg(ax)),
            ret,
            mov(imm(0), reg(ax)),
            ret
        ]),
        function(foo, [
            allocate_stack(16),
            mov(reg(di), stack(-4)),
            mov(reg(si), stack(-8)),
            mov(stack(-4), reg(r10)),
            mov(reg(r10), stack(-12)),
            mov(stack(-8), reg(r10)),
            binary(sub, reg(r10), stack(-12)),
            mov(stack(-12), reg(ax)),
            ret,
            mov(imm(0), reg(ax)),
            ret
        ])
    ])).

test(callee_copy_args_regs) :-
    callee_copy_args([a, b, c], [Is, []]),
    assertion(Is = [
        mov(reg(di), pseudo(a)),
        mov(reg(si), pseudo(b)),
        mov(reg(dx), pseudo(c))
    ]).

test(callee_copy_args_mem) :-
    callee_copy_args([a, b, c, d, e, f, g, h, i], [RIs, MIs]),
    assertion(RIs = [
        mov(reg(di), pseudo(a)),
        mov(reg(si), pseudo(b)),
        mov(reg(dx), pseudo(c)),
        mov(reg(cx), pseudo(d)),
        mov(reg(r8), pseudo(e)),
        mov(reg(r9), pseudo(f))
    ]),
    assertion(MIs = [
        mov(stack(16), pseudo(g)),
        mov(stack(24), pseudo(h)),
        mov(stack(32), pseudo(i))
    ]).

test(caller_copy_args_mem) :-
    caller_copy_args([
        constant(0),
        constant(1),
        constant(2),
        constant(3),
        constant(4),
        constant(5),
        constant(6),
        var('var.test.7'),
        constant(8)
    ], Is, []),
    assertion(Is = [
        mov(imm(0), reg(di)),
        mov(imm(1), reg(si)),
        mov(imm(2), reg(dx)),
        mov(imm(3), reg(cx)),
        mov(imm(4), reg(r8)),
        mov(imm(5), reg(r9)),
        push(imm(6)),
        mov(pseudo('var.test.7'), reg(ax)),
        push(reg(ax)),
        push(imm(8))
    ]).

:- end_tests(codegen).

