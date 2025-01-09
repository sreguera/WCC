/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(tacky,
    [ is_tacky/1,   % Succeeds if Tacky is a valid tacky program.
      tack/2        % Translates an Ast to Tacky.
    ]).
:- use_module(semantics, [is_valid_ast/1]).

/** <module> Tacky
 
Tacky generator for Chapter 8 of "Writing a C Compiler".

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
    % Always return 0 at the end as a precaution against path with no return.
    block_insts(Body, Instructions, [return(constant(0))]).

block_insts(block(Items), I0, Is) :-
    items_insts(Items, I0, Is).

items_insts([Item|Items], I0, Is) :-
    item_insts(Item, I0, I1),
    items_insts(Items, I1, Is).
items_insts([], Is, Is).

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
stmt_insts(if(Cond, Then, none), I0, Is) :-
    exp_insts(Cond, Result, I0, I1),
    I1 = [jump_if_zero(Result, EndLabel)|I2],
    stmt_insts(Then, I2, I3),
    I3 = [label(EndLabel)|Is],
    gensym('if_end', EndLabel).
stmt_insts(if(Cond, Then, Else), I0, Is) :-
    Else \= none,
    exp_insts(Cond, Result, I0, I1),
    I1 = [jump_if_zero(Result, ElseLabel)|I2],
    stmt_insts(Then, I2, I3),
    I3 = [jump(EndLabel), label(ElseLabel)|I4],
    stmt_insts(Else, I4, I5),
    I5 = [label(EndLabel)|Is],
    gensym('if_else', ElseLabel),
    gensym('if_end', EndLabel).
stmt_insts(compound(Block), I0, Is) :-
    block_insts(Block, I0, Is).
stmt_insts(break(Label), I0, Is) :-
    atom_concat(Label, '.break', BreakLabel),
    I0 = [jump(BreakLabel)|Is].
stmt_insts(continue(Label), I0, Is) :-
    atom_concat(Label, '.continue', ContinueLabel),
    I0 = [jump(ContinueLabel)|Is].
stmt_insts(while(Exp, Stmt, Label), I0, Is) :-
    I0 = [label(ContinueLabel)|I1],
    exp_insts(Exp, Result, I1, I2),
    I2 = [jump_if_zero(Result, BreakLabel)|I3],
    stmt_insts(Stmt, I3, I4),
    I4 = [
        jump(ContinueLabel),
        label(BreakLabel)
        | Is
    ],
    atom_concat(Label, '.break', BreakLabel),
    atom_concat(Label, '.continue', ContinueLabel).    
stmt_insts(do_while(Stmt, Exp, Label), I0, Is) :-
    I0 = [label(StartLabel)|I1],
    stmt_insts(Stmt, I1, I2),
    I2 = [label(ContinueLabel)|I3],
    exp_insts(Exp, Result, I3, I4),
    I4 = [
        jump_if_not_zero(Result, StartLabel),
        label(BreakLabel)
        | Is
    ],
    atom_concat(Label, '.start', StartLabel),
    atom_concat(Label, '.break', BreakLabel),
    atom_concat(Label, '.continue', ContinueLabel).    
stmt_insts(for(Init, Cond, Post, Stmt, Label), I0, Is) :-
    for_init_insts(Init, I0, I1),
    I1 = [label(StartLabel)|I2],
    for_cond_insts(Cond, BreakLabel, I2, I3),
    stmt_insts(Stmt, I3, I4),
    I4 = [label(ContinueLabel)|I5],
    for_post_insts(Post, I5, I6),
    I6 = [
        jump(StartLabel),
        label(BreakLabel)
        | Is
    ],
    atom_concat(Label, '.start', StartLabel),
    atom_concat(Label, '.break', BreakLabel),
    atom_concat(Label, '.continue', ContinueLabel).    
stmt_insts(switch(Exp, Stmt, Label, Cases), I0, Is) :-
    exp_insts(Exp, Result, I0, I1),
    cases_insts(Cases, BreakLabel, Result, I1, I2),
    stmt_insts(Stmt, I2, I3),
    I3 = [label(BreakLabel)|Is],
    atom_concat(Label, '.break', BreakLabel).
stmt_insts(case(_Exp, Stmt, Label), I0, Is) :-
    I0 = [label(Label)|I1],
    stmt_insts(Stmt, I1, Is).
stmt_insts(default(Stmt, Label), I0, Is) :-
    I0 = [label(Label)|I1],
    stmt_insts(Stmt, I1, Is).
stmt_insts(goto(Label), I0, Is) :-
    I0 = [jump(Label)|Is].
stmt_insts(labelled(Label, Stmt), I0, Is) :-
    I0 = [label(Label)|I1],
    stmt_insts(Stmt, I1, Is).
stmt_insts(null, Is, Is).

for_init_insts(init_decl(Decl), I0, Is) :-
    decl_insts(Decl, I0, Is).
for_init_insts(init_exp(Exp), I0, Is) :-
    (   Exp = none
    ->  I0 = Is
    ;   exp_insts(Exp, _, I0, Is)
    ).

for_cond_insts(none, _, Is, Is) :- !.
for_cond_insts(Cond, BreakLabel, I0, Is) :-
    exp_insts(Cond, Result, I0, I1),
    I1 = [jump_if_zero(Result, BreakLabel)|Is].

for_post_insts(none, Is, Is) :- !.
for_post_insts(Post, I0, Is) :-
    exp_insts(Post, _, I0, Is). 

cases_insts([], Break, _, I0, Is) :-
    I0 = [jump(Break)|Is].
cases_insts([Case|Rest], Break, Result, I0, Is) :-
    case_insts(Case, Result, I0, I1),
    cases_insts(Rest, Break, Result, I1, Is).

case_insts(case(Exp, Label), Result, I0, Is) :-
    mk_tmpvar(Dest),
    I0 = [
        binary(equal, Exp, Result, Dest),
        jump_if_not_zero(Dest, Label)
        | Is
    ].
case_insts(default(Label), _Result, I0, Is) :-
    I0 = [jump(Label)|Is].

exp_insts(constant(Int), constant(Int), Is, Is).
exp_insts(var(Id), var(Id), Is, Is).
exp_insts(unary(Op, Inner), Dest, I0, Is) :-
    unary_insts(Op, Inner, Dest, I0, Is).
exp_insts(binary(Op, Left, Right), Dest, I0, Is) :-
    binary_insts(Op, Left, Right, Dest, I0, Is).
exp_insts(assignment(Var, Exp), Var, I0, Is) :-
    exp_insts(Exp, Dest, I0, I1),
    I1 = [copy(Dest, Var)|Is].
exp_insts(conditional(Cond, Then, Else), Dest, I0, Is) :-
    exp_insts(Cond, Result, I0, I1),
    I1 = [jump_if_zero(Result, ElseLabel)|I2],
    exp_insts(Then, DestThen, I2, I3),
    I3 = [
        copy(DestThen, Dest),
        jump(EndLabel),
        label(ElseLabel)
        | I4
    ],
    exp_insts(Else, DestElse, I4, I5),
    I5 = [
        copy(DestElse, Dest),
        label(EndLabel)
        | Is
    ],
    mk_tmpvar(Dest),
    gensym('if_else', ElseLabel),
    gensym('if_end', EndLabel).

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
    ProgramIn = program(function(main, block([
        d(declaration('var.a.1', unary(negate, constant(5)))),
        d(declaration('var.b.2', none)),
        s(expression(assignment(var('var.b.2'), binary(subtract, var('var.a.1'), constant(3))))),
        s(return(var('var.b.2')))
    ]))),
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

