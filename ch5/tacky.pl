/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(tacky,
    [ is_tacky/1,   % Succeeds if Tacky is a valid tacky program.
      tack/2        % Translates an Ast to Tacky.
    ]).
:- use_module(semantics, [is_valid_ast/1]).

/** <module> Tacky
 
Tacky generator for Chapter 5 of "Writing a C Compiler".

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
is_tacky_instruction(copy(Src, Dst)) :-
    is_tacky_value(Src),
    is_tacky_value(Dst).
is_tacky_instruction(jump(Identifier)) :-
    atom(Identifier).
is_tacky_instruction(jump_if_zero(Condition, Identifier)) :-
    is_tacky_value(Condition),
    atom(Identifier).
is_tacky_instruction(jump_if_not_zero(Condition, Identifier)) :-
    is_tacky_value(Condition),
    atom(Identifier).
is_tacky_instruction(label(Identifier)) :-
    atom(Identifier).

is_tacky_value(constant(Val)) :-
    integer(Val).
is_tacky_value(var(Id)) :-
    atom(Id).

is_tacky_uni_op(complement).
is_tacky_uni_op(negate).
is_tacky_uni_op(not).

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
is_tacky_bin_op(equal).
is_tacky_bin_op(not_equal).
is_tacky_bin_op(less_than).
is_tacky_bin_op(less_eq).
is_tacky_bin_op(greater_than).
is_tacky_bin_op(greater_eq).

%!  tack(+Ast, -Tacky)
%
%   Translates an Ast to Tacky.

tack(Program0, Program) :-
    assertion(is_valid_ast(Program0)),
    tack_program(Program0, Program).

tack_program(program(FunDef), program(FunDefTacky)) :-
    reset_gensym,
    tack_function(FunDef, FunDefTacky).

tack_function(function(Name, Body), function(Name, Instructions)) :-
    body_insts(Body, Instructions, []).

body_insts([Item|Items], I0, Is) :-
    item_insts(Item, I0, I1),
    body_insts(Items, I1, Is).
% Always return 0 at the end as a precaution against path with no return.
body_insts([], [return(constant(0))|Is], Is).

item_insts(d(Decl), I0, Is) :-
    decl_insts(Decl, I0, Is).
item_insts(s(Stmt), I0, Is) :-
    stmt_insts(Stmt, I0, Is).

decl_insts(declaration(Var, Exp), I0, Is) :-
    (   Exp = none
    ->  I0 = Is
    ;   exp_insts(Exp, Result, I0, I1),
        I1 = [copy(Result, var(Var))|Is]
    ).

stmt_insts(return(Exp), I0, Is) :-
    exp_insts(Exp, Result, I0, I1),
    I1 = [return(Result)|Is].
stmt_insts(expression(Exp), I0, Is) :-
    exp_insts(Exp, _Result, I0, Is).
stmt_insts(null, Is, Is).

exp_insts(constant(Int), constant(Int), Is, Is).
exp_insts(var(Id), var(Id), Is, Is).
exp_insts(unary(Op, Inner), Dest, I0, Is) :-
    unary_insts(Op, Inner, Dest, I0, Is).
exp_insts(binary(Op, Left, Right), Dest, I0, Is) :-
    binary_insts(Op, Left, Right, Dest, I0, Is).
exp_insts(assignment(Var, Exp), Var, I0, Is) :-
    exp_insts(Exp, Dest, I0, I1),
    I1 = [copy(Dest, Var)|Is].

unary_insts(pre_incr, Inner, Dest0, I0, Is) :- !,
    exp_insts(Inner, Dest0, I0, I1),
    I1 = [binary(add, Dest0, constant(1), Dest0)|Is].
unary_insts(pre_decr, Inner, Dest0, I0, Is) :- !,
    exp_insts(Inner, Dest0, I0, I1),
    I1 = [binary(subtract, Dest0, constant(1), Dest0)|Is].
unary_insts(post_incr, Inner, Dest, I0, Is) :- !,
    exp_insts(Inner, Dest0, I0, I1),
    mk_tmpvar(Dest),
    I1 = [copy(Dest0, Dest), binary(add, Dest0, constant(1), Dest0)|Is].
unary_insts(post_decr, Inner, Dest, I0, Is) :- !,
    exp_insts(Inner, Dest0, I0, I1),
    mk_tmpvar(Dest),
    I1 = [copy(Dest0, Dest), binary(subtract, Dest0, constant(1), Dest0)|Is].
unary_insts(Op, Inner, Dest, I0, Is) :-
    exp_insts(Inner, Dest0, I0, I1),
    mk_tmpvar(Dest),
    I1 = [unary(Op, Dest0, Dest)|Is].

binary_insts(and, Left, Right, Dest, I0, Is) :- !, %TODO replace ! with index on op
    exp_insts(Left, DestL, I0, I1),
    I1 = [jump_if_zero(DestL, FalseLabel)|I2],
    exp_insts(Right, DestR, I2, I3),
    I3 = [
        jump_if_zero(DestR, FalseLabel),
        copy(constant(1), Dest),
        jump(EndLabel),
        label(FalseLabel),
        copy(constant(0), Dest),
        label(EndLabel)
        | Is
    ],
    mk_tmpvar(Dest),
    gensym('and_end', EndLabel),
    gensym('and_false', FalseLabel).
binary_insts(or, Left, Right, Dest, I0, Is) :- !,
    exp_insts(Left, DestL, I0, I1),
    I1 = [jump_if_not_zero(DestL, TrueLabel)|I2],
    exp_insts(Right, DestR, I2, I3),
    I3 = [
        jump_if_not_zero(DestR, TrueLabel),
        copy(constant(0), Dest),
        jump(EndLabel),
        label(TrueLabel),
        copy(constant(1), Dest),
        label(EndLabel)
        | Is
    ],
    mk_tmpvar(Dest),
    gensym('or_end', EndLabel),
    gensym('or_true', TrueLabel).
binary_insts(Op, Left, Right, Dest, I0, Is) :-
    Op \= and,
    Op \= or,
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

test(p5) :-
    ProgramIn = program(function(main, [
        d(declaration('var.a.1', unary(negate, constant(5)))),
        d(declaration('var.b.2', none)),
        s(expression(assignment(var('var.b.2'), binary(subtract, var('var.a.1'), constant(3))))),
        s(return(var('var.b.2')))
    ])),
    tack(ProgramIn, Tacky),
    assertion(is_tacky(Tacky)),
    assertion(Tacky = program(function(main, [
        unary(negate, constant(5), var('tmp.1')),
        copy(var('tmp.1'), var('var.a.1')),
        binary(subtract, var('var.a.1'), constant(3), var('tmp.2')),
        copy(var('tmp.2'), var('var.b.2')),
        return(var('var.b.2')),
        return(constant(0))
    ]))).

test(and) :-
    reset_gensym,
    exp_insts(binary(and, constant(8), constant(0)), _Dest, Is, []),
    Is = [
        jump_if_zero(constant(8), and_false1),
        jump_if_zero(constant(0), and_false1),
        copy(constant(1), var('tmp.1')),
        jump(and_end1),
        label(and_false1),
        copy(constant(0), var('tmp.1')),
        label(and_end1)
    ].

test(or) :-
    reset_gensym,
    exp_insts(binary(or, constant(8), constant(0)), _Dest, Is, []),
    Is = [
        jump_if_not_zero(constant(8), or_true1),
        jump_if_not_zero(constant(0), or_true1),
        copy(constant(0), var('tmp.1')),
        jump(or_end1),
        label(or_true1),
        copy(constant(1), var('tmp.1')),
        label(or_end1)
    ].

:- end_tests(tacky).

