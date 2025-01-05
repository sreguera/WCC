/* Copyright 2025 José Sebastián Reguera Candal
*/
:- module(semantics,
    [ is_valid_ast/1,   % Succeeds if Ast is a validated AST.
      validate/2        % Transforms the input program into a validated one.
    ]).
:- use_module(parser, [is_ast/1]).


/** <module> Parser
 
Semantic analysis for Chapter 9 of "Writing a C Compiler".

*/

%!  is_valid_ast(+Ast)
%
%   Succeeds if Ast is a validated AST.

is_valid_ast(Ast) :-
    is_valid_program_ast(Ast).

is_valid_program_ast(program(FunctionDeclarations)) :-
    forall(member(Decl, FunctionDeclarations), is_valid_fun_decl_ast(Decl)).

is_valid_block_ast(block(Items)) :-
    forall(member(Item, Items), is_valid_block_item_ast(Item)).

is_valid_block_item_ast(s(Statement)) :-
    is_valid_statement_ast(Statement).
is_valid_block_item_ast(d(Declaration)) :-
    is_valid_declaration_ast(Declaration).

is_valid_declaration_ast(var_decl(Name, Item)) :-
    is_valid_var_decl_ast(var_decl(Name, Item)).
is_valid_declaration_ast(fun_decl(Name, Params, Block)) :-
    is_valid_fun_decl_ast(fun_decl(Name, Params, Block)).

is_valid_var_decl_ast(var_decl(Name, Item)) :-
    atom(Name),
    is_valid_opt_exp_ast(Item).

is_valid_fun_decl_ast(fun_decl(Name, Params, Block)) :-
    atom(Name),
    forall(member(Param, Params), atom(Param)),
    (   Block = none
    ->  true
    ;   is_valid_block_ast(Block)
    ).

is_valid_statement_ast(return(Exp)) :-
    is_valid_exp_ast(Exp).
is_valid_statement_ast(expression(Exp)) :-
    is_valid_exp_ast(Exp).
is_valid_statement_ast(if(Condition, Then, Else)) :-
    is_valid_exp_ast(Condition),
    is_valid_statement_ast(Then),
    (   Else = none
    ->  true
    ;   is_valid_statement_ast(Else)
    ).
is_valid_statement_ast(compound(Block)) :-
    is_valid_block_ast(Block).
is_valid_statement_ast(break(Label)) :-
    atom(Label).
is_valid_statement_ast(continue(Label)) :-
    atom(Label).
is_valid_statement_ast(while(Exp, Stmt, Label)) :-
    is_valid_exp_ast(Exp),
    is_valid_statement_ast(Stmt),
    atom(Label).
is_valid_statement_ast(do_while(Stmt, Exp, Label)) :-
    is_valid_statement_ast(Stmt),
    is_valid_exp_ast(Exp),
    atom(Label).
is_valid_statement_ast(for(Init, Cond, Post, Stmt, Label)) :-
    is_valid_for_init_ast(Init),
    is_valid_opt_exp_ast(Cond),
    is_valid_opt_exp_ast(Post),
    is_valid_statement_ast(Stmt),
    atom(Label).
is_valid_statement_ast(switch(Exp, Stmt, Label, Cases)) :-
    is_valid_exp_ast(Exp),
    is_valid_statement_ast(Stmt),
    atom(Label),
    forall(member(Case, Cases), is_valid_case_ast(Case)).
is_valid_statement_ast(case(Exp, Stmt, Label)) :-
    is_valid_exp_ast(Exp),
    is_valid_statement_ast(Stmt),
    atom(Label).
is_valid_statement_ast(default(Stmt, Label)) :-
    is_valid_statement_ast(Stmt),
    atom(Label).
is_valid_statement_ast(goto(Label)) :-
    atom(Label).
is_valid_statement_ast(labelled(Label, Stmt)) :-
    atom(Label),
    is_valid_statement_ast(Stmt).
is_valid_statement_ast(null).

is_valid_for_init_ast(init_decl(Decl)) :-
    is_valid_declaration_ast(Decl).
is_valid_for_init_ast(init_exp(Exp)) :-
    is_valid_opt_exp_ast(Exp).

is_valid_case_ast(case(Exp, Label)) :-
    is_valid_exp_ast(Exp),
    atom(Label).
is_valid_case_ast(default(Label)) :-
    atom(Label).

is_valid_exp_ast(constant(Value)) :-
    integer(Value).
is_valid_exp_ast(var(Id)) :-
    atom(Id).
is_valid_exp_ast(funcall(Id, Args)) :-
    atom(Id),
    forall(member(Arg, Args), is_valid_exp_ast(Arg)).
is_valid_exp_ast(assignment(Left, Right)) :-
    is_valid_exp_ast(Left),
    is_valid_exp_ast(Right).
is_valid_exp_ast(unary(Op, Exp)) :-
    is_valid_unary_op(Op),
    is_valid_exp_ast(Exp).
is_valid_exp_ast(binary(Op, Left, Right)) :-
    is_valid_bin_op(Op),
    is_valid_exp_ast(Left),
    is_valid_exp_ast(Right).
is_valid_exp_ast(conditional(Condition, Left, Right)) :-
    is_valid_exp_ast(Condition),
    is_valid_exp_ast(Left),
    is_valid_exp_ast(Right).

is_valid_opt_exp_ast(Exp) :-
    (   Exp = none
    ->  true
    ;   is_valid_exp_ast(Exp)
    ).

is_valid_unary_op(negate).
is_valid_unary_op(complement).
is_valid_unary_op(not).
is_valid_unary_op(pre_incr).
is_valid_unary_op(pre_decr).
is_valid_unary_op(post_incr).
is_valid_unary_op(post_decr).

is_valid_bin_op(multiply).
is_valid_bin_op(divide).
is_valid_bin_op(remainder).
is_valid_bin_op(add).
is_valid_bin_op(subtract).
is_valid_bin_op(lshift).
is_valid_bin_op(rshift).
is_valid_bin_op(less_than).
is_valid_bin_op(less_eq).
is_valid_bin_op(greater_than).
is_valid_bin_op(greater_eq).
is_valid_bin_op(equal).
is_valid_bin_op(not_equal).
is_valid_bin_op(bit_and).
is_valid_bin_op(bit_xor).
is_valid_bin_op(bit_or).
is_valid_bin_op(and).
is_valid_bin_op(or).
is_valid_bin_op(cond).

%!  validate(+Program, -ValidatedProgram)
%
%   Transforms the input program into a validated one.

validate(Program0, Program) :-
    assertion(is_ast(Program0)),
    validate_program(Program0, Program1),
    type_check_program(Program1),
    label_program(Program1),
    loop_program(Program1, Program2),
    gather_program(Program2, Program).


%---------------------------%
%   IDENTIFIER RESOLUTION   %
%---------------------------%

validate_program(program(FunDecls), program(ValFunDecls)) :-
    reset_gensym,
    empty_table(S0),
    table_push_scope(S0, S1),
    foldl(validate_decl, FunDecls, ValFunDecls, S1, _S).

validate_block(block(Items), block(ValItems), S0, S0) :-
    table_push_scope(S0, S1),
    foldl(validate_item, Items, ValItems, S1, _).

validate_function_block(none, none, S0, S0).
validate_function_block(block(Items), block(ValItems), S0, S0) :-
    % A function will create and pass the parameters as the initial scope. 
    foldl(validate_item, Items, ValItems, S0, _).

validate_item(d(Decl), d(ValDecl), S0, S) :-
    (   Decl = fun_decl(Name, _Args, block(_))
    ->  throw(local_function_definition(Name))
    ;   validate_decl(Decl, ValDecl, S0, S)
    ).    
validate_item(s(Stmt), s(ValStmt), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S).

%!  validate_decl(+Decl, -ValDecl, +SIn, -SOut)
%
%   Validates the identifiers in a declaration.

validate_decl(var_decl(Name, Exp), var_decl(UniqueName, ValExp), S0, S) :-
    (   table_get_local_entry(S0, Name, _)
    ->  throw(duplicated_var(Name))
    ;   mk_varname(Name, UniqueName),
        table_add_entry(S0, Name, entry(UniqueName, none), S1),
        validate_exp_opt(Exp, ValExp, S1, S)
    ).
validate_decl(fun_decl(Name, Params, Block), fun_decl(UniqueName, ValParams, ValBlock), S0, S1) :-
    (   table_get_local_entry(S0, Name, entry(_, none))
    ->  throw(duplicated_declaration(Name))
    ;   Name = UniqueName,
        table_add_entry(S0, Name, entry(UniqueName, external), S1),
        table_push_scope(S1, S2),
        foldl(validate_param, Params, ValParams, S2, S3),
        validate_function_block(Block, ValBlock, S3, _)
    ).

validate_param(Name, UniqueName, S0, S) :-
    (   table_get_local_entry(S0, Name, _)
    ->  throw(duplicated_var(Name))
    ;   mk_varname(Name, UniqueName),
        table_add_entry(S0, Name, entry(UniqueName, none), S)
    ).

%!  validate_stmt(+Stmt, -ValStmt, +SIn, -SOut)
%
%   Validates the identifiers in a statement.

validate_stmt(return(Exp), return(ValExp), S0, S) :-
    validate_exp(Exp, ValExp, S0, S).
validate_stmt(expression(Exp), expression(ValExp), S0, S) :-
    validate_exp(Exp, ValExp, S0, S).
validate_stmt(if(Cond, Then, Else), if(ValCond, ValThen, ValElse), S0, S) :-
    validate_exp(Cond, ValCond, S0, S1),
    validate_stmt(Then, ValThen, S1, S2),
    (   Else = none
    ->  S = S2, 
        ValElse = none
    ;   validate_stmt(Else, ValElse, S2, S)
    ).
validate_stmt(compound(Block), compound(ValBlock), S0, S) :-
    validate_block(Block, ValBlock, S0, S).
validate_stmt(break, break, S, S).
validate_stmt(continue, continue, S, S).
validate_stmt(while(Exp, Stmt), while(ValExp, ValStmt), S0, S) :-
    validate_exp(Exp, ValExp, S0, S1),
    validate_stmt(Stmt, ValStmt, S1, S).
validate_stmt(do_while(Stmt, Exp), do_while(ValStmt, ValExp), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S1),
    validate_exp(Exp, ValExp, S1, S).    
validate_stmt(for(Init, Cond, Post, Stmt), for(ValInit, ValCond, ValPost, ValStmt), S0, S0) :-
    table_push_scope(S0, S1),
    validate_for_init(Init, ValInit, S1, S2),
    validate_exp_opt(Cond, ValCond, S2, S3),
    validate_exp_opt(Post, ValPost, S3, S4),
    validate_stmt(Stmt, ValStmt, S4, _S).
validate_stmt(switch(Exp, Stmt), switch(ValExp, ValStmt), S0, S) :-
    validate_exp(Exp, ValExp, S0, S1),
    validate_stmt(Stmt, ValStmt, S1, S).
validate_stmt(case(Exp, Stmt), case(ValExp, ValStmt), S0, S) :-
    validate_exp(Exp, ValExp, S0, S1),
    validate_stmt(Stmt, ValStmt, S1, S).
validate_stmt(default(Stmt), default(ValStmt), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S).
validate_stmt(goto(Label), goto(Label), S, S).
validate_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S0, S) :-
    validate_stmt(Stmt, ValStmt, S0, S).
validate_stmt(null, null, S, S).

validate_for_init(init_decl(Decl), init_decl(ValDecl), S0, S) :-
    validate_decl(Decl, ValDecl, S0, S).
validate_for_init(init_exp(Exp), init_exp(ValExp), S0, S) :-
    validate_exp_opt(Exp, ValExp, S0, S).

%!  validate_exp(+Exp, -ValExp, +SIn, -SOut)
%
%   Validates the identifiers in an expression.

validate_exp(constant(Int), constant(Int), S, S).
validate_exp(var(Name), var(UniqueName), S, S) :-
    (   table_get_global_entry(S, Name, entry(UniqueName, _))
    ->  true
    ;   throw(undefined_variable(Name))
    ).
validate_exp(funcall(Name, Args), funcall(UniqueName, ValArgs), S0, S) :-
    (   table_get_global_entry(S0, Name, entry(UniqueName, _))
    ->  true
    ;   throw(undefined_function(Name))
    ),
    foldl(validate_exp, Args, ValArgs, S0, S).
validate_exp(unary(Op, Val), unary(Op, ValVal), S0, S) :-
    (   memberchk(Op, [pre_incr, pre_decr, post_incr, post_decr]),
        Val \= var(_)
    ->  throw(invalid_lvalue(Val))
    ;   true
    ),
    validate_exp(Val, ValVal, S0, S).
validate_exp(binary(Op, Left, Right), binary(Op, ValLeft, ValRight), S0, S) :-
    validate_exp(Left, ValLeft, S0, S1),
    validate_exp(Right, ValRight, S1, S).
validate_exp(assignment(Var, Val), assignment(ValVar, ValVal), S0, S) :-
    (   Var \= var(_)
    ->  throw(invalid_lvalue(Var))
    ;   validate_exp(Var, ValVar, S0, S1),
        validate_exp(Val, ValVal, S1, S)
    ).
validate_exp(conditional(Cond, Then, Else), conditional(ValCond, ValThen, ValElse), S0, S) :-
    validate_exp(Cond, ValCond, S0, S1),
    validate_exp(Then, ValThen, S1, S2),
    validate_exp(Else, ValElse, S2, S).

validate_exp_opt(none, none, S, S) :-
    !.
validate_exp_opt(Exp, ValExp, S0, S) :-
    Exp \= none,
    validate_exp(Exp, ValExp, S0, S).

mk_varname(Name, UniqueName) :-
    gensym('var.', Unique),
    atom_concat('var.', Id, Unique),
    atomic_list_concat(['var', Name, Id], '.', UniqueName).


%------------------%
%   TYPE CHECKER   %
%------------------%

type_check_program(program(FunDecls)) :-
    empty_table(S0),
    table_push_scope(S0, S1),
    foldl(type_check_decl, FunDecls, S1, _S).

type_check_block(block(Items), S0, S) :-
    table_push_scope(S0, S1),
    foldl(type_check_item, Items, S1, S2),
    table_replace_base(S0, S2, S).

type_check_function_block(none, S0, S0).
type_check_function_block(block(Items), S0, S) :-
    % A function will create and pass the parameters as the initial scope. 
    foldl(type_check_item, Items, S0, S1),
    table_replace_base(S0, S1, S).

type_check_item(d(Decl), S0, S) :-
    (   Decl = fun_decl(Name, _Args, block(_))
    ->  throw(local_function_definition(Name))
    ;   type_check_decl(Decl, S0, S)
    ).    
type_check_item(s(Stmt), S0, S) :-
    type_check_stmt(Stmt, S0, S).

%!  type_check_decl(+Decl, +SIn, -SOut)
%
%   Validates the identifiers in a declaration.

type_check_decl(var_decl(Name, Exp), S0, S) :-
    table_add_entry(S0, Name, entry(Name, int), S1),
    type_check_exp_opt(Exp, S1, S).
type_check_decl(fun_decl(Name, Params, Block), S0, S) :-
    length(Params, NumParams),
    (   Block = none
    ->  Defined = undefined
    ;   Defined = defined
    ),
    (   table_get_global_entry(S0, Name, Entry)
    ->  (   Entry = entry(Name, fun(NumParams), PrevDefined)
        ->  (   (PrevDefined, Defined) = (defined, defined)
            ->  throw(multiple_definitions(Name))
            ;   merge_def(PrevDefined, Defined, NewDefined),
                table_add_base_entry(S0, Name, entry(Name, fun(NumParams), NewDefined), S1)
            )
        ;   throw(incompatible_declaration(Name))
        )
    ;   table_add_base_entry(S0, Name, entry(Name, fun(NumParams), Defined), S1)
    ),
    table_push_scope(S1, S2),
    foldl(type_check_param, Params, S2, S3),
    type_check_function_block(Block, S3, S4),
    table_replace_base(S1, S4, S).

merge_def(defined,     defined,   defined) :- !.
merge_def(defined,   undefined,   defined) :- !.
merge_def(undefined,   defined,   defined) :- !.
merge_def(undefined, undefined, undefined).

type_check_param(Name, S0, S) :-
    table_add_entry(S0, Name, entry(Name, int), S).

%!  type_check_stmt(+Stmt, +SIn, -SOut)
%
%   Validates the identifiers in a statement.

type_check_stmt(return(Exp), S0, S) :-
    type_check_exp(Exp, S0, S).
type_check_stmt(expression(Exp), S0, S) :-
    type_check_exp(Exp, S0, S).
type_check_stmt(if(Cond, Then, Else), S0, S) :-
    type_check_exp(Cond, S0, S1),
    type_check_stmt(Then, S1, S2),
    (   Else = none
    ->  S = S2
    ;   type_check_stmt(Else, S2, S)
    ).
type_check_stmt(compound(Block), S0, S) :-
    type_check_block(Block, S0, S).
type_check_stmt(break, S, S).
type_check_stmt(continue, S, S).
type_check_stmt(while(Exp, Stmt), S0, S) :-
    type_check_exp(Exp, S0, S1),
    type_check_stmt(Stmt, S1, S).
type_check_stmt(do_while(Stmt, Exp), S0, S) :-
    type_check_stmt(Stmt, S0, S1),
    type_check_exp(Exp, S1, S).    
type_check_stmt(for(Init, Cond, Post, Stmt), S0, S0) :-
    table_push_scope(S0, S1),
    type_check_for_init(Init, S1, S2),
    type_check_exp_opt(Cond, S2, S3),
    type_check_exp_opt(Post, S3, S4),
    type_check_stmt(Stmt, S4, _S).
type_check_stmt(switch(Exp, Stmt), S0, S) :-
    type_check_exp(Exp, S0, S1),
    type_check_stmt(Stmt, S1, S).
type_check_stmt(case(Exp, Stmt), S0, S) :-
    type_check_exp(Exp, S0, S1),
    type_check_stmt(Stmt, S1, S).
type_check_stmt(default(Stmt), S0, S) :-
    type_check_stmt(Stmt, S0, S).
type_check_stmt(goto(_Label), S, S).
type_check_stmt(labelled(_Label, Stmt), S0, S) :-
    type_check_stmt(Stmt, S0, S).
type_check_stmt(null, S, S).

type_check_for_init(init_decl(Decl), S0, S) :-
    type_check_decl(Decl, S0, S).
type_check_for_init(init_exp(Exp), S0, S) :-
    type_check_exp_opt(Exp, S0, S).

%!  type_check_exp(+Exp, +SIn, -SOut)
%
%   Validates the identifiers in an expression.

type_check_exp(constant(_Int), S, S).
type_check_exp(var(Name), S, S) :-
    (   table_get_global_entry(S, Name, Entry)
    ->  (   Entry = entry(Name, fun(_), _)
        ->  throw(function_used_as_variable(Name))
        ;   true
        )
    ;   throw(undefined_variable(Name))
    ).
type_check_exp(funcall(Name, Args), S0, S) :-
    (   table_get_global_entry(S0, Name, Entry)
    ->  (   Entry = entry(Name, int)
        ->  throw(variable_used_as_function(Name))
        ;   Entry = entry(Name, fun(NumArgs), _),
            length(Args, NumArgs)
        ->  true
        ;   throw(wrong_number_of_args(Name))
        )
    ;   throw(undefined_function(Name))
    ),
    foldl(type_check_exp, Args, S0, S).
type_check_exp(unary(_Op, Val), S0, S) :-
    type_check_exp(Val, S0, S).
type_check_exp(binary(_Op, Left, Right), S0, S) :-
    type_check_exp(Left, S0, S1),
    type_check_exp(Right, S1, S).
type_check_exp(assignment(Var, Val), S0, S) :-
    type_check_exp(Var, S0, S1),
    type_check_exp(Val, S1, S).
type_check_exp(conditional(Cond, Then, Else), S0, S) :-
    type_check_exp(Cond, S0, S1),
    type_check_exp(Then, S1, S2),
    type_check_exp(Else, S2, S).

type_check_exp_opt(none, S, S) :-
    !.
type_check_exp_opt(Exp, S0, S) :-
    Exp \= none,
    type_check_exp(Exp, S0, S).


%------------------%
%   SCOPED TABLE   %
%------------------%

empty_table([]).

table_push_scope(Table0, [Scope|Table0]) :-
    empty_assoc(Scope).

table_add_entry(Table0, Key, Value, Table) :-
    Table0 = [Scope0|Rest],
    put_assoc(Key, Scope0, Value, Scope1),
    Table = [Scope1|Rest].

table_add_base_entry(Table0, Key, Value, Table) :-
    reverse(Table0, Table1),
    Table1 = [Scope0|Rest],
    put_assoc(Key, Scope0, Value, Scope1),
    Table2 = [Scope1|Rest],
    reverse(Table2, Table).

table_replace_base(Table0, Table1, Table2) :-
    reverse(Table0, [_Base0|Rest0]),
    reverse(Table1, [Base1|_Rest1]),
    reverse([Base1|Rest0], Table2).

table_get_local_entry(Table, Key, Value) :-
    Table = [Scope|_],
    get_assoc(Key, Scope, Value).

table_get_global_entry(Table, Key, Value) :-
    Table = [Scope|Rest],
    (   get_assoc(Key, Scope, Value)
    ->  true
    ;   table_get_global_entry(Rest, Key, Value)
    ).


%----------------------%
%   LABEL RESOLUTION   %
%----------------------%

label_program(program(FunDecls)) :-
    maplist(label_function, FunDecls).

label_function(fun_decl(_Name, _Params, Body)) :-
    S0 = state([], []),
    (   Body = none
    ->  true
    ;   label_block(Body, S0, state(Labels, Gotos)),
        subtract(Gotos, Labels, Undefined),
        (   Undefined = [L|_]
        ->  throw(undefined_label(L))
        ;   true
        )
    ).
   
label_block(block(Items), S0, S) :-
    foldl(label_item, Items, S0, S).

label_item(d(_Decl), S, S).
label_item(s(Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).

label_stmt(return(_Exp), S, S).
label_stmt(expression(_Exp), S, S).
label_stmt(if(_Cond, Then, Else), S0, S) :-
    label_stmt(Then, S0, S1),
    (   Else = none
    ->  S = S1
    ;   label_stmt(Else, S1, S)
    ).
label_stmt(compound(Block), S0, S) :-
    label_block(Block, S0, S).
label_stmt(break, S, S).
label_stmt(continue, S, S).
label_stmt(while(_Exp, Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
label_stmt(do_while(Stmt, _Exp), S0, S) :-
    label_stmt(Stmt, S0, S).    
label_stmt(for(_Init, _Cond, _Post, Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
label_stmt(switch(_Exp, Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
label_stmt(case(_Exp, Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
label_stmt(default(Stmt), S0, S) :-
    label_stmt(Stmt, S0, S).
label_stmt(goto(Label), S0, S) :-
    S0 = state(Labels, Gotos),
    (   memberchk(Label, Gotos)
    ->  true
    ;   S = state(Labels, [Label|Gotos])
    ).
label_stmt(labelled(Label, Stmt), S0, S) :-
    S0 = state(Labels, Gotos),
    (   memberchk(Label, Labels)
    ->  throw(duplicated_label(Label))
    ;   S1 = state([Label|Labels], Gotos),
        label_stmt(Stmt, S1, S)
    ).
label_stmt(null, S, S).


%--------------------%
%   LOOP LABELLING   %
%--------------------%

loop_program(program(FunDecls), program(ValFunDecls)) :-
    reset_gensym,
    maplist(loop_function, FunDecls, ValFunDecls).

loop_function(fun_decl(Name, Args, Body), fun_decl(Name, Args, ValBody)) :-
    loop_block(Body, ValBody, []).

loop_block(none, none, _).
loop_block(block(Items), block(ValItems), S) :-
    maplist(loop_item_1(S), Items, ValItems).

loop_item_1(S, Item0, Item) :-
    loop_item(Item0, Item, S).

loop_item(d(Decl), d(Decl), _).
loop_item(s(Stmt), s(ValStmt), S) :-
    loop_stmt(Stmt, ValStmt, S).

loop_stmt(return(Exp), return(Exp), _).
loop_stmt(expression(Exp), expression(Exp), _).
loop_stmt(if(Cond, Then, Else), if(Cond, ValThen, ValElse), S) :-
    loop_stmt(Then, ValThen, S),
    (   Else = none
    ->  ValElse = none
    ;   loop_stmt(Else, ValElse, S)
    ).
loop_stmt(compound(Block), compound(ValBlock), S) :-
    loop_block(Block, ValBlock, S).
loop_stmt(break, break(Label), S) :-
    (   S = [loop(Label)|_]
    ->  true
    ;   S = [switch(Label)|_]
    ->  true
    ;   throw(break_outside_loop)
    ).
loop_stmt(continue, continue(Label), S) :-
    (   memberchk(loop(Label), S)
    ->  true
    ;   throw(continue_outside_loop)
    ).
loop_stmt(while(Exp, Stmt), while(Exp, ValStmt, Label), S) :-
    gensym(loop, Label),
    loop_stmt(Stmt, ValStmt, [loop(Label)|S]).
loop_stmt(do_while(Stmt, Exp), do_while(ValStmt, Exp, Label), S) :-
    gensym(loop, Label),
    loop_stmt(Stmt, ValStmt, [loop(Label)|S]).    
loop_stmt(for(Init, Cond, Post, Stmt), for(Init, Cond, Post, ValStmt, Label), S) :-
    gensym(loop, Label),
    loop_stmt(Stmt, ValStmt, [loop(Label)|S]).
loop_stmt(switch(Exp, Stmt), switch(Exp, ValStmt, Label), S) :-
    gensym(switch, Label),
    loop_stmt(Stmt, ValStmt, [switch(Label)|S]).
loop_stmt(case(Exp, Stmt), case(Exp, ValStmt, Label), S) :-
    (   memberchk(switch(_), S)
    ->  true
    ;   throw(case_without_switch)
    ),
    gensym(case, Label),
    loop_stmt(Stmt, ValStmt, S).
loop_stmt(default(Stmt), default(ValStmt, Label), S) :-
    (   memberchk(switch(_), S)
    ->  true
    ;   throw(default_without_switch)
    ),
    gensym(default, Label),
    loop_stmt(Stmt, ValStmt, S).
loop_stmt(goto(Label), goto(Label), _).
loop_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S) :-
    loop_stmt(Stmt, ValStmt, S).
loop_stmt(null, null, _).


%-------------------------%
%   GATHER SWITCH CASES   %
%-------------------------%

gather_program(program(FunDecls), program(ValFunDecls)) :-
    maplist(gather_function, FunDecls, ValFunDecls).

gather_function(fun_decl(Name, Args, Body), fun_decl(Name, Args, ValBody)) :-
    gather_block(Body, ValBody, [], []).

gather_block(none, none, S, S).
gather_block(block(Items), block(ValItems), S0, S) :-
    foldl(gather_item, Items, ValItems, S0, S).

gather_item(d(Decl), d(Decl), S, S).
gather_item(s(Stmt), s(ValStmt), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).

%!  gather_stmt(+Stmt, -ValStmt, +SIn, -SOut)
%
%   gathers the variables in a statement.

gather_stmt(return(Exp), return(Exp), S, S).
gather_stmt(expression(Exp), expression(Exp), S, S).
gather_stmt(if(Cond, Then, Else), if(Cond, ValThen, ValElse), S0, S) :-
    gather_stmt(Then, ValThen, S0, S1),
    (   Else = none
    ->  S = S1,
        ValElse = none
    ;   gather_stmt(Else, ValElse, S1, S)
    ).
gather_stmt(compound(Block), compound(ValBlock), S0, S) :-
    gather_block(Block, ValBlock, S0, S).
gather_stmt(break(Label), break(Label), S, S).
gather_stmt(continue(Label), continue(Label), S, S).
gather_stmt(while(Exp, Stmt, Label), while(Exp, ValStmt, Label), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(do_while(Stmt, Exp, Label), do_while(ValStmt, Exp, Label), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).    
gather_stmt(for(Init, Cond, Post, Stmt, Label), for(Init, Cond, Post, ValStmt, Label), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(switch(Exp, Stmt, Label), switch(Exp, ValStmt, Label, RS), S0, S0) :-
    gather_stmt(Stmt, ValStmt, [], S),
    switch_check(S),
    reverse(S, RS).
gather_stmt(case(Exp, Stmt, Label), case(Exp, ValStmt, Label), S0, [case(Exp, Label)|S]) :-
    (   Exp = constant(_)
    ->  true
    ;   throw(non_constant_case)
    ),
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(default(Stmt, Label), default(ValStmt, Label), S0, [default(Label)|S]) :-
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(goto(Label), goto(Label), S, S).
gather_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S0, S) :-
    gather_stmt(Stmt, ValStmt, S0, S).
gather_stmt(null, null, S, S).

switch_check(S) :-
    maplist(without_label, S, S1),
    sort(0, @=<, S1, S2),
    clumped(S2, S3),
    sort(2, @>=, S3, S4),
    (   S4 = [X-Y|_],
        Y > 1
    ->  throw(duplicate_switch_clause(X))
    ;   true
    ).

without_label(default(_), default).
without_label(case(Exp, _), case(Exp)).


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(semantics).

test(validate) :-
    ProgramIn = program([fun_decl(main, [], block([
        d(var_decl(a, constant(5))),
        d(var_decl(b, none)),
        s(expression(assignment(var(b), binary(subtract, var(a), constant(3))))),
        s(return(var(b)))
    ]))]),
    validate(ProgramIn, ProgramOut),
    assertion(is_valid_ast(ProgramOut)),
    assertion(ProgramOut = program([fun_decl(main, [], block([
        d(var_decl('var.a.1', constant(5))),
        d(var_decl('var.b.2', none)),
        s(expression(assignment(var('var.b.2'), binary(subtract, var('var.a.1'), constant(3))))),
        s(return(var('var.b.2')))
    ]))])).

test(validate2) :-
    ProgramIn = program([
        fun_decl(foo, [a, b], none),
        fun_decl(main, [], block([s(return(funcall(foo, [constant(2), constant(1)])))])),
        fun_decl(foo, [x, y], block([s(return(binary(subtract, var(x), var(y))))]))
    ]),
    validate(ProgramIn, ProgramOut),
    assertion(is_valid_ast(ProgramOut)),
    assertion(ProgramOut = program([
        fun_decl(foo, ['var.a.1', 'var.b.2'], none),
        fun_decl(main, [], block([s(return(funcall(foo, [constant(2), constant(1)])))])),
        fun_decl(foo, ['var.x.3', 'var.y.4'], block([s(return(binary(subtract, var('var.x.3'), var('var.y.4'))))]))
    ])).

:- end_tests(semantics).