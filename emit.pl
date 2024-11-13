:- module(emit, [emit/2]).

emit(Fragment, Output) :-
    with_output_to(string(Output), emit(Fragment)).
   
emit(program(FunDef)) :-
    emit(FunDef),
    format("    .section .note.GNU-stack,\"\",@progbits~n").
emit(function(Name, Instructions)) :-
    format("    .globl ~s~n", [Name]),
    format("~s:~n", [Name]),
    maplist(emit, Instructions).
emit(mov(Source, Dest)) :-
    exp_emit(Source, SOut),
    exp_emit(Dest, EOut),
    format("    movl ~s, ~s~n", [SOut, EOut]).
emit(ret) :-
    format("    ret~n").

exp_emit(reg, "%eax").
exp_emit(imm(Int), Out) :-
    format(string(Out), "$~d", [Int]).

:- begin_tests(emit).

test(emit) :-
    emit(program(function(main, [mov(imm(2), reg), ret])), Asm),
    Asm = "    .globl main
main:
    movl $2, %eax
    ret
    .section .note.GNU-stack,\"\",@progbits
".

:- end_tests(emit).

