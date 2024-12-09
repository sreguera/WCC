/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(tacky, [tack/2]).

/** <module> Tacky
 
Tacky generator for Chapter 8 of "Writing a C Compiler".

The generator convertes the AST to Tacky IL.
    * Program = program(FunctionDefinition)
    * FunctionDefinition = function(Name:atom, Body:[instruction])
    * Instruction = 
        | return(val)
        | unary(Unop, src:val, dst:val)
        | binary(Binop, left:val, right:val, dst:val)
        | copy(src:val, dst:val)
        | jump(identifier)
        | jump_if_zero(condition, identifier)
        | jump_if_not_zero(condition, identifier)
        | label(identifier)
    * Val = 
        | constant(Value:int)
        | var(Name:atom)
    * Unop = complement | negate | not
    * Binop = add | subtract | multiply | divide | remainder
        | bit_and | bit_or | bit_xor | lshift | rshift
        | equal | not_equal
        | less_than | less_eq | greater_than | greater_eq
*/

tack(program(FunDef), program(FunDefTacky)) :-
    reset_gensym,
    tack(FunDef, FunDefTacky).

tack(function(Name, Body), function(Name, Instructions)) :-
    block_insts(Body, Instructions, [return(constant(0))]).

block_insts(block(Items), Insts, T) :-
    items_insts(Items, Insts, T).

items_insts([Item|Items], Insts, T) :-
    item_insts(Item, Insts, I1),
    items_insts(Items, I1, T).
items_insts([], T, T).

item_insts(d(Decl), Insts, T) :-
    decl_insts(Decl, Insts, T).
item_insts(s(Stmt), Insts, T) :-
    stmt_insts(Stmt, Insts, T).

decl_insts(declaration(Var, Exp), Insts, T) :-
    (   Exp = none
    ->  Insts = T
    ;   exp_insts(Exp, Result, Insts, I1),
        I1 = [copy(Result, var(Var))|T]
    ).

stmt_insts(return(Exp), Insts, T) :-
    exp_insts(Exp, Result, Insts, I1),
    I1 = [return(Result)|T].
stmt_insts(expression(Exp), Insts, T) :-
    exp_insts(Exp, _Result, Insts, T).
stmt_insts(if(Cond, Then, none), Insts, T) :-
    exp_insts(Cond, Result, Insts, I1),
    I1 = [jump_if_zero(Result, EndLabel)|I2],
    stmt_insts(Then, I2, I3),
    I3 = [label(EndLabel)|T],
    gensym('if_end', EndLabel).
stmt_insts(if(Cond, Then, Else), Insts, T) :-
    Else \= none,
    exp_insts(Cond, Result, Insts, I1),
    I1 = [jump_if_zero(Result, ElseLabel)|I2],
    stmt_insts(Then, I2, I3),
    I3 = [jump(EndLabel), label(ElseLabel)|I4],
    stmt_insts(Else, I4, I5),
    I5 = [label(EndLabel)|T],
    gensym('if_else', ElseLabel),
    gensym('if_end', EndLabel).
stmt_insts(compound(Block), Insts, T) :-
    block_insts(Block, Insts, T).
stmt_insts(break(Label), Insts, T) :-
    atom_concat(Label, '.break', BreakLabel),
    Insts = [jump(BreakLabel)|T].
stmt_insts(continue(Label), Insts, T) :-
    atom_concat(Label, '.continue', ContinueLabel),
    Insts = [jump(ContinueLabel)|T].
stmt_insts(while(Exp, Stmt, Label), Insts, T) :-
    Insts = [label(ContinueLabel)|I1],
    exp_insts(Exp, Result, I1, I2),
    I2 = [jump_if_zero(Result, BreakLabel)|I3],
    stmt_insts(Stmt, I3, I4),
    I4 = [
        jump(ContinueLabel),
        label(BreakLabel)
        | T
    ],
    atom_concat(Label, '.break', BreakLabel),
    atom_concat(Label, '.continue', ContinueLabel).    
stmt_insts(do_while(Stmt, Exp, Label), Insts, T) :-
    Insts = [label(StartLabel)|I1],
    stmt_insts(Stmt, I1, I2),
    I2 = [label(ContinueLabel)|I3],
    exp_insts(Exp, Result, I3, I4),
    I4 = [
        jump_if_not_zero(Result, StartLabel),
        label(BreakLabel)
        | T
    ],
    atom_concat(Label, '.start', StartLabel),
    atom_concat(Label, '.break', BreakLabel),
    atom_concat(Label, '.continue', ContinueLabel).    
stmt_insts(for(Init, Cond, Post, Stmt, Label), Insts, T) :-
    for_init_insts(Init, Insts, I1),
    I1 = [label(StartLabel)|I2],
    for_cond_insts(Cond, BreakLabel, I2, I3),
    stmt_insts(Stmt, I3, I4),
    I4 = [label(ContinueLabel)|I5],
    for_post_insts(Post, I5, I6),
    I6 = [
        jump(StartLabel),
        label(BreakLabel)
        | T
    ],
    atom_concat(Label, '.start', StartLabel),
    atom_concat(Label, '.break', BreakLabel),
    atom_concat(Label, '.continue', ContinueLabel).    
stmt_insts(goto(Label), Insts, T) :-
    Insts = [jump(Label)|T].
stmt_insts(labelled(Label, Stmt), Insts, T) :-
    Insts = [label(Label)|I1],
    stmt_insts(Stmt, I1, T).
stmt_insts(null, T, T).

for_init_insts(init_decl(Decl), Is, T) :-
    decl_insts(Decl, Is, T).
for_init_insts(init_exp(Exp), Is, T) :-
    (   Exp = none
    ->  Is = T
    ;   exp_insts(Exp, _, Is, T)
    ).

for_cond_insts(none, _, T, T) :- !.
for_cond_insts(Cond, BreakLabel, Is, T) :-
    exp_insts(Cond, Result, Is, I1),
    I1 = [jump_if_zero(Result, BreakLabel)|T].

for_post_insts(none, T, T) :- !.
for_post_insts(Post, Is, T) :-
    exp_insts(Post, _, Is, T). 

exp_insts(constant(Int), constant(Int), T, T).
exp_insts(var(Id), var(Id), T, T).
exp_insts(unary(Op, Inner), Dest, Is, T) :-
    unary_insts(Op, Inner, Dest, Is, T).
exp_insts(binary(Op, Left, Right), Dest, Is, T) :-
    binary_insts(Op, Left, Right, Dest, Is, T).
exp_insts(assignment(Var, Exp), Var, Is, T) :-
    exp_insts(Exp, Dest, Is, I1),
    I1 = [copy(Dest, Var)|T].
exp_insts(conditional(Cond, Then, Else), Dest, Is, T) :-
    exp_insts(Cond, Result, Is, I1),
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
        | T
    ],
    mk_tmpvar(Dest),
    gensym('if_else', ElseLabel),
    gensym('if_end', EndLabel).

unary_insts(pre_incr, Inner, Dest0, Is, T) :- !,
    exp_insts(Inner, Dest0, Is, I1),
    I1 = [binary(add, Dest0, constant(1), Dest0)|T].
unary_insts(pre_decr, Inner, Dest0, Is, T) :- !,
    exp_insts(Inner, Dest0, Is, I1),
    I1 = [binary(subtract, Dest0, constant(1), Dest0)|T].
unary_insts(post_incr, Inner, Dest, Is, T) :- !,
    exp_insts(Inner, Dest0, Is, I1),
    mk_tmpvar(Dest),
    I1 = [copy(Dest0, Dest), binary(add, Dest0, constant(1), Dest0)|T].
unary_insts(post_decr, Inner, Dest, Is, T) :- !,
    exp_insts(Inner, Dest0, Is, I1),
    mk_tmpvar(Dest),
    I1 = [copy(Dest0, Dest), binary(subtract, Dest0, constant(1), Dest0)|T].
unary_insts(Op, Inner, Dest, Is, T) :-
    exp_insts(Inner, Dest0, Is, I1),
    mk_tmpvar(Dest),
    I1 = [unary(Op, Dest0, Dest)|T].

binary_insts(and, Left, Right, Dest, Is, T) :- !, %TODO replace ! with index on op
    exp_insts(Left, DestL, Is, I1),
    I1 = [jump_if_zero(DestL, FalseLabel)|I2],
    exp_insts(Right, DestR, I2, I3),
    I3 = [
        jump_if_zero(DestR, FalseLabel),
        copy(constant(1), Dest),
        jump(EndLabel),
        label(FalseLabel),
        copy(constant(0), Dest),
        label(EndLabel)
        | T
    ],
    mk_tmpvar(Dest),
    gensym('and_end', EndLabel),
    gensym('and_false', FalseLabel).
binary_insts(or, Left, Right, Dest, Is, T) :- !,
    exp_insts(Left, DestL, Is, I1),
    I1 = [jump_if_not_zero(DestL, TrueLabel)|I2],
    exp_insts(Right, DestR, I2, I3),
    I3 = [
        jump_if_not_zero(DestR, TrueLabel),
        copy(constant(0), Dest),
        jump(EndLabel),
        label(TrueLabel),
        copy(constant(1), Dest),
        label(EndLabel)
        | T
    ],
    mk_tmpvar(Dest),
    gensym('or_end', EndLabel),
    gensym('or_true', TrueLabel).
binary_insts(Op, Left, Right, Dest, Is, T) :-
    Op \= and,
    Op \= or,
    exp_insts(Left, DestL, Is, I1),
    exp_insts(Right, DestR, I1, I2),
    mk_tmpvar(Dest),
    I2 = [binary(Op, DestL, DestR, Dest)|T].

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
    Tacky = program(function(main, [
        unary(negate, constant(5), var('tmp.1')),
        copy(var('tmp.1'), var('var.a.1')),
        binary(subtract, var('var.a.1'), constant(3), var('tmp.2')),
        copy(var('tmp.2'), var('var.b.2')),
        return(var('var.b.2')),
        return(constant(0))
    ])).

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

