/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(tacky, [tack/2]).
:- use_module(semantics, [is_valid_ast/1]).

/** <module> Tacky
 
Tacky generator for Chapter 7 of "Writing a C Compiler".

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
    assertion(is_valid_ast(program(FunDef))),
    reset_gensym,
    tack(FunDef, FunDefTacky).

tack(function(Name, Body), function(Name, Instructions)) :-
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
stmt_insts(goto(Label), I0, Is) :-
    I0 = [jump(Label)|Is].
stmt_insts(labelled(Label, Stmt), I0, Is) :-
    I0 = [label(Label)|I1],
    stmt_insts(Stmt, I1, Is).
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

