/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(emit, [emit/2]).
:- use_module(codegen, [is_assembly/1]).

/** <module> Emit
 
Assembler emitter for Chapter 2 of "Writing a C Compiler".

*/

emit(Asm, Output) :-
    assertion(is_assembly(Asm)),
    with_output_to(string(Output), emit(Asm)).
   
emit(program(FunDef)) :-
    emit(FunDef),
    format("    .section .note.GNU-stack,\"\",@progbits~n").
emit(function(Name, Instructions)) :-
    format("    .globl ~s~n", [Name]),
    format("~s:~n", [Name]),
    format("    pushq   %rbp~n"),
    format("    movq    %rsp, %rbp~n"),
    maplist(inst_emit, Instructions).

inst_emit(mov(Source, Dest)) :-
    exp_emit(Source, SOut),
    exp_emit(Dest, EOut),
    format("    movl    ~s, ~s~n", [SOut, EOut]).
inst_emit(ret) :-
    format("    movq    %rbp, %rsp~n"),
    format("    popq    %rbp~n"),
    format("    ret~n").
inst_emit(unary(Op, Val)) :-
    op_emit(Op, OpOut),
    exp_emit(Val, VOut),
    format("    ~s  ~s~n", [OpOut, VOut]).
inst_emit(allocate_stack(Size)) :-
    format("    subq    $~d, %rsp~n", [Size]).

op_emit(not, "notl").
op_emit(neg, "negl").

exp_emit(reg(R), Out) :-
    reg_emit(R, Out).
exp_emit(stack(Offset), Out) :-
    format(string(Out), "~d(%rbp)", [Offset]).
exp_emit(imm(Int), Out) :-
    format(string(Out), "$~d", [Int]).

reg_emit(ax, "%eax").
reg_emit(r10, "%r10d").


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(emit).

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
    Asm = "    .globl main
main:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $8, %rsp
    movl    $2, -4(%rbp)
    negl  -4(%rbp)
    movl    -4(%rbp), %r10d
    movl    %r10d, -8(%rbp)
    notl  -8(%rbp)
    movl    -8(%rbp), %eax
    movq    %rbp, %rsp
    popq    %rbp
    ret
    .section .note.GNU-stack,\"\",@progbits
".

:- end_tests(emit).

