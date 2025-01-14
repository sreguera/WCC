/* Copyright 2025 José Sebastián Reguera Candal
*/
:- module(emit, [emit/2]).
:- use_module(codegen, [is_assembly/1]).

/** <module> Emit
 
Assembler emitter for Chapter 9 of "Writing a C Compiler".

*/

emit(Asm, Output) :-
    assertion(is_assembly(Asm)),
    with_output_to(string(Output), emit_program(Asm)).
   
emit_program(program(FunDefs)) :-
    maplist(emit_function, FunDefs),
    format("    .section .note.GNU-stack,\"\",@progbits~n").

emit_function(function(Name, Instructions)) :-
    format("    .globl ~s~n", [Name]),
    format("~s:~n", [Name]),
    format("    pushq   %rbp~n"),
    format("    movq    %rsp, %rbp~n"),
    maplist(inst_emit, Instructions).

inst_emit(mov(Source, Dest)) :-
    exp_emit_32(Source, SOut),
    exp_emit_32(Dest, EOut),
    format("    movl    ~s, ~s~n", [SOut, EOut]).
inst_emit(ret) :-
    format("    movq    %rbp, %rsp~n"),
    format("    popq    %rbp~n"),
    format("    ret~n").
inst_emit(cdq) :-
    format("    cdq~n").
inst_emit(idiv(Val)) :-
    exp_emit_32(Val, VOut),
    format("    idivl   ~s~n", [VOut]).
inst_emit(unary(Op, Val)) :-
    op_emit(Op, OpOut),
    exp_emit_32(Val, VOut),
    format("    ~s  ~s~n", [OpOut, VOut]).
inst_emit(binary(Op, Val1, Val2)) :-
    op_emit(Op, OpOut),
    exp_emit_32(Val1, V1Out),
    exp_emit_32(Val2, V2Out),
    format("    ~s  ~s, ~s~n", [OpOut, V1Out, V2Out]).
inst_emit(cmp(Val1, Val2)) :-
    exp_emit_32(Val1, V1Out),
    exp_emit_32(Val2, V2Out),
    format("    cmpl    ~s, ~s~n", [V1Out, V2Out]).
inst_emit(jmp(Label)) :-
    format("    jmp     .L~s~n", [Label]).
inst_emit(jmp_cc(Cond, Label)) :-
    cond_suffix(Cond, Suffix),
    format("    j~s     .L~s~n", [Suffix, Label]).
inst_emit(set_cc(Cond, Val)) :-
    cond_suffix(Cond, Suffix),
    exp_emit_8(Val, VOut),
    format("    set~s   ~s~n", [Suffix, VOut]).
inst_emit(label(Label)) :-
    format(".L~s:~n", [Label]).
inst_emit(allocate_stack(Size)) :-
    format("    subq    $~d, %rsp~n", [Size]).
inst_emit(deallocate_stack(Size)) :-
    format("    addq    $~d, %rsp~n", [Size]).
inst_emit(push(Val)) :-
    exp_emit_64(Val, ValOut),
    format("    pushq    ~s~n", [ValOut]).
inst_emit(call(Label)) :-
    format("    call    ~s@PLT~n", [Label]).
inst_emit(X) :-
    throw(invalid_inst(X)).

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

cond_suffix(e,  "e").
cond_suffix(ne, "ne").
cond_suffix(l,  "l").
cond_suffix(le, "le").
cond_suffix(g,  "g").
cond_suffix(ge, "ge").

exp_emit_64(reg(R), Out) :-
    reg_emit_64(R, Out).
exp_emit_64(stack(Offset), Out) :-
    format(string(Out), "~d(%rbp)", [Offset]).
exp_emit_64(imm(Int), Out) :-
    format(string(Out), "$~d", [Int]).

exp_emit_32(reg(R), Out) :-
    reg_emit_32(R, Out).
exp_emit_32(stack(Offset), Out) :-
    format(string(Out), "~d(%rbp)", [Offset]).
exp_emit_32(imm(Int), Out) :-
    format(string(Out), "$~d", [Int]).

exp_emit_8(reg(R), Out) :-
    reg_emit_8(R, Out).
exp_emit_8(stack(Offset), Out) :-
    format(string(Out), "~d(%rbp)", [Offset]).
exp_emit_8(imm(Int), Out) :-
    format(string(Out), "$~d", [Int]).

reg_emit_64(ax,  "%rax").
reg_emit_64(cx,  "%rcx").
reg_emit_64(dx,  "%rdx").
reg_emit_64(di,  "%rdi").
reg_emit_64(si,  "%rsi").
reg_emit_64(r8,  "%r8").
reg_emit_64(r9,  "%r9").
reg_emit_64(r10, "%r10").
reg_emit_64(r11, "%r11").

reg_emit_32(ax,  "%eax").
reg_emit_32(cx,  "%ecx").
reg_emit_32(dx,  "%edx").
reg_emit_32(di,  "%edi").
reg_emit_32(si,  "%esi").
reg_emit_32(r8,  "%r8d").
reg_emit_32(r9,  "%r9d").
reg_emit_32(r10, "%r10d").
reg_emit_32(r11, "%r11d").

reg_emit_8(ax,  "%al").
reg_emit_8(cx,  "%cl").
reg_emit_8(dx,  "%dl").
reg_emit_8(di,  "%dil").
reg_emit_8(si,  "%sil").
reg_emit_8(r8,  "%r8b").
reg_emit_8(r9,  "%r9b").
reg_emit_8(r10, "%r10b").
reg_emit_8(r11, "%r11b").


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(emit).

test(inst_emit) :-
    with_output_to(string(Output), inst_emit(binary(mult, imm(3), reg(r11)))),
    Output = "    imull  $3, %r11d\n".

test(inst_emit) :-
    with_output_to(string(Output), inst_emit(set_cc(ne, reg(ax)))),
    Output = "    setne   %al\n".

test(emit) :-
    emit(program([function(main, [
        allocate_stack(8),
        mov(imm(2), stack(-4)),
        unary(neg, stack(-4)),
        mov(stack(-4), reg(r10)),
        mov(reg(r10), stack(-8)),
        unary(not, stack(-8)),
        mov(stack(-8), reg(ax)),
        ret
    ])]), Asm),
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

