/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(emitter, [emit/2]).
:- use_module(codegen, [is_assembly/1]).

/** <module> Emitter
 
Assembler emitter for Chapter 3 of "Writing a C Compiler".

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
    format("~4|pushq ~12|%rbp~n"),
    format("~4|movq ~12|%rsp, %rbp~n"),
    maplist(inst_emit, Instructions).

inst_emit(mov(Source, Dest)) :-
    exp_emit(Source, SOut),
    exp_emit(Dest, EOut),
    format("~4|movl ~12|~s, ~s~n", [SOut, EOut]).
inst_emit(ret) :-
    format("~4|movq ~12|%rbp, %rsp~n"),
    format("~4|popq ~12|%rbp~n"),
    format("~4|ret~n").
inst_emit(cdq) :-
    format("~4|cdq~n").
inst_emit(idiv(Val)) :-
    exp_emit(Val, VOut),
    format("~4|idivl ~12|~s~n", [VOut]).
inst_emit(unary(Op, Val)) :-
    op_emit(Op, OpOut),
    exp_emit(Val, VOut),
    format("~4|~s ~12|~s~n", [OpOut, VOut]).
inst_emit(binary(Op, Val1, Val2)) :-
    op_emit(Op, OpOut),
    exp_emit(Val1, V1Out),
    exp_emit(Val2, V2Out),
    format("~4|~s ~12|~s, ~s~n", [OpOut, V1Out, V2Out]).
inst_emit(allocate_stack(Size)) :-
    format("~4|subq ~12|$~d, %rsp~n", [Size]).

op_emit(not, "notl").
op_emit(neg, "negl").
op_emit(add, "addl").
op_emit(sub, "subl").
op_emit(mult, "imull").
op_emit(and, "andl").
op_emit(or,  "orl").
op_emit(xor, "xorl").
op_emit(sal, "sall").
op_emit(sar, "sarl").

exp_emit(reg(R), Out) :-
    reg_emit(R, Out).
exp_emit(stack(Offset), Out) :-
    format(string(Out), "~d(%rbp)", [Offset]).
exp_emit(imm(Int), Out) :-
    format(string(Out), "$~d", [Int]).

reg_emit(ax, "%eax").
reg_emit(cx, "%ecx").
reg_emit(dx, "%edx").
reg_emit(r10, "%r10d").
reg_emit(r11, "%r11d").


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(emit).

test(inst_emit) :-
    with_output_to(string(Output), inst_emit(binary(mult, imm(3), reg(r11)))),
    assertion(Output = "    imull   $3, %r11d\n").

test(emit) :-
    emit(program(function(main, [
        allocate_stack(8),
        mov(imm(2), stack(-4)),
        unary(neg, stack(-4)),
        mov(stack(-4), reg(r10)),
        mov(reg(r10), stack(-8)),
        unary(not, stack(-8)),
        mov(stack(-8), reg(ax)),
        ret
    ])), Asm),
    assertion(Asm = "    .globl main
main:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $8, %rsp
    movl    $2, -4(%rbp)
    negl    -4(%rbp)
    movl    -4(%rbp), %r10d
    movl    %r10d, -8(%rbp)
    notl    -8(%rbp)
    movl    -8(%rbp), %eax
    movq    %rbp, %rsp
    popq    %rbp
    ret
    .section .note.GNU-stack,\"\",@progbits
").

:- end_tests(emit).

