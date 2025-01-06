/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(tacky,
    [ is_tacky/1,   % Succeeds if Tacky is a valid tacky program.
      tack/2        % Translates an Ast to Tacky.
    ]).
:- use_module(parser, [is_ast/1]).


/** <module> Tacky
 
Tacky generator for Chapter 3 of "Writing a C Compiler".

The generator convertes the AST to Tacky IL.

*/

%!  is_tacky(+Tacky)
%
%   Succeeds if Tacky is a valid tacky program.

is_tacky(Program) :-
    is_tacky_program(Program).

is_tacky_program(program(FunDef)) :-
    is_tacky_function(FunDef).

is_tacky_function(function(Name, Body)) :-
    atom(Name),
    forall(member(Inst, Body), is_tacky_instruction(Inst)).

is_tacky_instruction(return(Val)) :-
    is_tacky_value(Val).
is_tacky_instruction(unary(Op, Src, Dst)) :-
    is_tacky_uni_op(Op),
    is_tacky_value(Src),
    is_tacky_value(Dst).
is_tacky_instruction(binary(Op, Arg1, Arg2, Dst)) :-
    is_tacky_bin_op(Op),
    is_tacky_value(Arg1),
    is_tacky_value(Arg2),
    is_tacky_value(Dst).

is_tacky_value(constant(Val)) :-
    integer(Val).
is_tacky_value(var(Id)) :-
    atom(Id).

is_tacky_uni_op(complement).
is_tacky_uni_op(negate).

is_tacky_bin_op(add).
is_tacky_bin_op(subtract).
is_tacky_bin_op(multiply).
is_tacky_bin_op(divide).
is_tacky_bin_op(remainder).
is_tacky_bin_op(bit_and).
is_tacky_bin_op(bit_or).
is_tacky_bin_op(bit_xor).
is_tacky_bin_op(lshift).
is_tacky_bin_op(rshift).

%!  tack(+Ast, -Tacky)
%
%   Translates an Ast to Tacky.

tack(Program0, Program) :-
    assertion(is_ast(Program0)),
    tack_program(Program0, Program).

tack_program(program(FunDef), program(FunDefTacky)) :-
    reset_gensym,
    tack_function(FunDef, FunDefTacky).

tack_function(function(Name, Body), function(Name, Instructions)) :-
    stmt_insts(Body, Instructions).

stmt_insts(return(Exp), I0) :-
    exp_insts(Exp, Result, I0, I1),
    I1 = [return(Result)].

exp_insts(constant(Int), constant(Int), Is, Is).
exp_insts(unary(Op, Inner), Dest, I0, Is) :-
    exp_insts(Inner, Dest0, I0, I1),
    mk_tmpvar(Dest),
    I1 = [unary(Op, Dest0, Dest)|Is].
exp_insts(binary(Op, Left, Right), Dest, I0, Is) :-
    exp_insts(Left, DestL, I0, I1),
    exp_insts(Right, DestR, I1, I2),
    mk_tmpvar(Dest),
    I2 = [binary(Op, DestL, DestR, Dest)|Is].

mk_tmpvar(var(UniqueName)) :-
    gensym('tmp.', UniqueName).


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(tacky).

test(p1) :-
    tack(program(function(main, return(constant(2)))), Tacky),
    assertion(is_tacky(Tacky)),
    assertion(Tacky = program(function(main, [return(constant(2))]))).

test(p2) :-
    tack(program(function(main, return(unary(negate, unary(complement, unary(negate, constant(2))))))), Tacky),
    assertion(is_tacky(Tacky)),
    assertion(Tacky = program(function(main, [
        unary(negate, constant(2), var('tmp.1')),
        unary(complement, var('tmp.1'), var('tmp.2')),
        unary(negate, var('tmp.2'), var('tmp.3')),
        return(var('tmp.3'))
    ]))).

test(p3a) :-
    tack(program(function(main,return(binary(add,constant(1),constant(2))))), Tacky),
    assertion(is_tacky(Tacky)),
    assertion(Tacky = program(function(main, [
        binary(add, constant(1), constant(2), var('tmp.1')),
        return(var('tmp.1'))
    ]))).

test(p3) :-
    tack(program(function(main, return(
        binary(add, constant(2), binary(multiply, constant(3), constant(4)))))), Tacky),
    assertion(is_tacky(Tacky)),
    assertion(Tacky = program(function(main, [
        binary(multiply, constant(3), constant(4), var('tmp.1')),
        binary(add, constant(2), var('tmp.1'), var('tmp.2')),
        return(var('tmp.2'))
    ]))).

:- end_tests(tacky).

