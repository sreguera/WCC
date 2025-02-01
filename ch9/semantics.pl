/* Copyright 2025 José Sebastián Reguera Candal
*/
:- module(semantics,
    [ is_valid_ast/1,   % Succeeds if Ast is a validated AST.
      validate/2        % Transforms the input program into a validated one.
    ]).
:- use_module(parser, [is_ast/1]).
:- use_module(scoped_table).


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

is_valid_declaration_ast(var_decl(Name, Init)) :-
    is_valid_var_decl_ast(var_decl(Name, Init)).
is_valid_declaration_ast(fun_decl(Name, Params, Block)) :-
    is_valid_fun_decl_ast(fun_decl(Name, Params, Block)).

is_valid_var_decl_ast(var_decl(Name, Init)) :-
    atom(Name),
    is_valid_opt_exp_ast(Init).

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
    resolve_program(Program0, Program1),
    type_check_program(Program1, Program2),
    label_program(Program2, Program3),
    loop_program(Program3, Program4),
    gather_program(Program4, Program).


%---------------------------%
%   IDENTIFIER RESOLUTION   %
%---------------------------%

%!  resolve_program(+Program, -ValProgram)
%
%   Resolves the identifiers in the program Program, producing ValProgram.
%   ValProgram is identical to Program except:
%   * There are no duplicate variables (same name in same scope).
%   * There are no duplicates function parameters.
%   * Variables have a unique global name.
%   * Increment operators have a variable as an argument.
%   * Assignment operators have a variable as left argument.
%
%   @throws duplicated_var(Name) if a variable or function parameter is duplicated.
%   @throws duplicated_declaration(Name) if a function name has already been used.
%   @throws undefined_variable(Name) if a variable is used without being defined.
%   @throws undefined_function(Name) if a function is used without being defined.
%   @throws invalid_lvalue(Expression) if an expression is used where a var is required.
%   @throws local_function_definition(Name) if a function is being defined in an inner scope.

resolve_program(program(FunDecls), program(ValFunDecls)) :-
    reset_gensym,
    empty_table(S0),
    table_push_scope(S0, S1),   % The root scope for function declarations.
    foldl(resolve_decl, FunDecls, ValFunDecls, S1, _S).

resolve_block(block(Items), block(ValItems), S0, S0) :-
    table_push_scope(S0, S1),   % Each block has its own scope.
    foldl(resolve_item, Items, ValItems, S1, _).

resolve_function_block(none, none, S0, S0).
resolve_function_block(block(Items), block(ValItems), S0, S0) :-
    % A function will create and pass the parameters as the initial scope. 
    foldl(resolve_item, Items, ValItems, S0, _).

resolve_item(d(Decl), d(ValDecl), S0, S) :-
    (   Decl = fun_decl(Name, _Params, block(_))
    ->  throw(local_function_definition(Name))
    ;   resolve_decl(Decl, ValDecl, S0, S)
    ).    
resolve_item(s(Stmt), s(ValStmt), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S).

%!  resolve_decl(+Decl, -ValDecl, +SIn, -SOut)
%
%   Resolves the identifiers in a declaration.

resolve_decl(var_decl(Name, Init), var_decl(UniqueName, ValInit), S0, S) :-
    (   table_get_local_entry(S0, Name, _)
    ->  throw(duplicated_var(Name))
    ;   mk_varname(Name, UniqueName),
        table_add_entry(S0, Name, entry(UniqueName, none), S1),
        resolve_exp_opt(Init, ValInit, S1, S)
    ).
resolve_decl(fun_decl(Name, Params, Block), fun_decl(UniqueName, ValParams, ValBlock), S0, S1) :-
    (   table_get_local_entry(S0, Name, entry(_, none))
    ->  throw(duplicated_declaration(Name))
    ;   Name = UniqueName,
        table_add_entry(S0, Name, entry(UniqueName, external), S1),
        table_push_scope(S1, S2),   % Function scope containing function parameters.
        foldl(resolve_param, Params, ValParams, S2, S3),
        resolve_function_block(Block, ValBlock, S3, _)
    ).

resolve_param(Name, UniqueName, S0, S) :-
    (   table_get_local_entry(S0, Name, _)
    ->  throw(duplicated_var(Name))
    ;   mk_varname(Name, UniqueName),
        table_add_entry(S0, Name, entry(UniqueName, none), S)
    ).

%!  resolve_stmt(+Stmt, -ValStmt, +SIn, -SOut)
%
%   Resolves the identifiers in a statement.

resolve_stmt(return(Exp), return(ValExp), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S).
resolve_stmt(expression(Exp), expression(ValExp), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S).
resolve_stmt(if(Cond, Then, Else), if(ValCond, ValThen, ValElse), S0, S) :-
    resolve_exp(Cond, ValCond, S0, S1),
    resolve_stmt(Then, ValThen, S1, S2),
    (   Else = none
    ->  S = S2, 
        ValElse = none
    ;   resolve_stmt(Else, ValElse, S2, S)
    ).
resolve_stmt(compound(Block), compound(ValBlock), S0, S) :-
    resolve_block(Block, ValBlock, S0, S).
resolve_stmt(break, break, S, S).
resolve_stmt(continue, continue, S, S).
resolve_stmt(while(Exp, Stmt), while(ValExp, ValStmt), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S1),
    resolve_stmt(Stmt, ValStmt, S1, S).
resolve_stmt(do_while(Stmt, Exp), do_while(ValStmt, ValExp), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S1),
    resolve_exp(Exp, ValExp, S1, S).    
resolve_stmt(for(Init, Cond, Post, Stmt), for(ValInit, ValCond, ValPost, ValStmt), S0, S0) :-
    table_push_scope(S0, S1),   % for has its own scope with the vars declared in Init.
    resolve_for_init(Init, ValInit, S1, S2),
    resolve_exp_opt(Cond, ValCond, S2, S3),
    resolve_exp_opt(Post, ValPost, S3, S4),
    resolve_stmt(Stmt, ValStmt, S4, _S).
resolve_stmt(switch(Exp, Stmt), switch(ValExp, ValStmt), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S1),
    resolve_stmt(Stmt, ValStmt, S1, S).
resolve_stmt(case(Exp, Stmt), case(ValExp, ValStmt), S0, S) :-
    resolve_exp(Exp, ValExp, S0, S1),
    resolve_stmt(Stmt, ValStmt, S1, S).
resolve_stmt(default(Stmt), default(ValStmt), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S).
resolve_stmt(goto(Label), goto(Label), S, S).
resolve_stmt(labelled(Label, Stmt), labelled(Label, ValStmt), S0, S) :-
    resolve_stmt(Stmt, ValStmt, S0, S).
resolve_stmt(null, null, S, S).

resolve_for_init(init_decl(Decl), init_decl(ValDecl), S0, S) :-
    resolve_decl(Decl, ValDecl, S0, S).
resolve_for_init(init_exp(Exp), init_exp(ValExp), S0, S) :-
    resolve_exp_opt(Exp, ValExp, S0, S).

%!  resolve_exp(+Exp, -ValExp, +SIn, -SOut)
%
%   Resolves the identifiers in an expression.

resolve_exp(constant(Int), constant(Int), S, S).
resolve_exp(var(Name), var(UniqueName), S, S) :-
    (   table_get_global_entry(S, Name, entry(UniqueName, _))
    ->  true
    ;   throw(undefined_variable(Name))
    ).
resolve_exp(funcall(Name, Args), funcall(UniqueName, ValArgs), S0, S) :-
    (   table_get_global_entry(S0, Name, entry(UniqueName, _))
    ->  true
    ;   throw(undefined_function(Name))
    ),
    foldl(resolve_exp, Args, ValArgs, S0, S).
resolve_exp(unary(Op, Val), unary(Op, ValVal), S0, S) :-
    (   memberchk(Op, [pre_incr, pre_decr, post_incr, post_decr]),
        Val \= var(_)
    ->  throw(invalid_lvalue(Val))
    ;   true
    ),
    resolve_exp(Val, ValVal, S0, S).
resolve_exp(binary(Op, Left, Right), binary(Op, ValLeft, ValRight), S0, S) :-
    resolve_exp(Left, ValLeft, S0, S1),
    resolve_exp(Right, ValRight, S1, S).
resolve_exp(assignment(Var, Val), assignment(ValVar, ValVal), S0, S) :-
    (   Var \= var(_)
    ->  throw(invalid_lvalue(Var))
    ;   resolve_exp(Var, ValVar, S0, S1),
        resolve_exp(Val, ValVal, S1, S)
    ).
resolve_exp(conditional(Cond, Then, Else), conditional(ValCond, ValThen, ValElse), S0, S) :-
    resolve_exp(Cond, ValCond, S0, S1),
    resolve_exp(Then, ValThen, S1, S2),
    resolve_exp(Else, ValElse, S2, S).

resolve_exp_opt(none, none, S, S) :-
    !.
resolve_exp_opt(Exp, ValExp, S0, S) :-
    Exp \= none,
    resolve_exp(Exp, ValExp, S0, S).

%!  mk_varname(+VarName, -UniqueName)
%
%   Generate a unique name for the given variable name.

mk_varname(Name, UniqueName) :-
    gensym('var.', Unique),
    atom_concat('var.', Id, Unique),
    atomic_list_concat(['var', Name, Id], '.', UniqueName).


%------------------%
%   TYPE CHECKER   %
%------------------%

%!  type_check_program(+Program, -ValProgram)
%
%   Type checks Program, producing ValProgram.
%
%   @throws local_function_definition(Name) if a function is being defined in an inner scope.
%   @throws multiple_definitions(Name) if ... TBW ...
%   @throws incompatible_declaration(Name) if ... TBW ...
%   @throws function_used_as_variable(Name) if ... TBW ...
%   @throws undefined_variable(Name) if ... TBW ...
%   @throws variable_used_as_function(Name) if ... TBW ...
%   @throws wrong_number_of_args(Name) if ... TBW ...
%   @throws undefined_function(Name) if ... TBW ...

type_check_program(program(FunDecls), program(FunDecls)) :-
    % No transformation right now, just checks.
    empty_table(S0),
    table_push_scope(S0, S1),   % The root scope for function declarations.
    foldl(type_check_decl, FunDecls, S1, _S).

type_check_block(block(Items), S0, S) :-
    table_push_scope(S0, S1),   % Each block has its own scope.
    foldl(type_check_item, Items, S1, S2),
    table_replace_base(S0, S2, S).

type_check_function_block(none, S0, S0).
type_check_function_block(block(Items), S0, S) :-
    % A function will create and pass the parameters as the initial scope. 
    foldl(type_check_item, Items, S0, S1),
    table_replace_base(S0, S1, S).

type_check_item(d(Decl), S0, S) :-
    (   Decl = fun_decl(Name, _Params, block(_))
    ->  throw(local_function_definition(Name))
    ;   type_check_decl(Decl, S0, S)
    ).    
type_check_item(s(Stmt), S0, S) :-
    type_check_stmt(Stmt, S0, S).

%!  type_check_decl(+Decl, +SIn, -SOut)
%
%   Validates the identifiers in a declaration.

type_check_decl(var_decl(Name, Init), S0, S) :-
    table_add_entry(S0, Name, entry(Name, int), S1),
    type_check_exp_opt(Init, S1, S).
type_check_decl(fun_decl(Name, Params, Block), S0, S) :-
    mk_fun_type(Params, FunType),
    (   Block = none
    ->  Defined = undefined
    ;   Defined = defined
    ),
    (   table_get_global_entry(S0, Name, entry(Name, PrevFunType, PrevDefined))
    ->  (   PrevFunType \= FunType
        ->  throw(incompatible_declaration(Name))
        ;   (PrevDefined, Defined) = (defined, defined)
        ->  throw(multiple_definitions(Name))
        ;   or_def(PrevDefined, Defined, NewDefined),
            table_add_base_entry(S0, Name, entry(Name, FunType, NewDefined), S1)
        )
    ;   table_add_base_entry(S0, Name, entry(Name, FunType, Defined), S1)
    ),
    table_push_scope(S1, S2),   % Function scope containing function parameters.
    foldl(type_check_param, Params, S2, S3),
    type_check_function_block(Block, S3, S4),
    table_replace_base(S1, S4, S).

type_check_param(Name, S0, S) :-
    table_add_entry(S0, Name, entry(Name, int), S).

mk_fun_type(Params, fun(NumParams)) :-
    length(Params, NumParams).

or_def(defined,     defined,   defined) :- !.
or_def(defined,   undefined,   defined) :- !.
or_def(undefined,   defined,   defined) :- !.
or_def(undefined, undefined, undefined).

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
    table_push_scope(S0, S1),   % for has its own scope with the vars declared in Init.
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


%----------------------%
%   LABEL RESOLUTION   %
%----------------------%

%!  label_program(+Program, -ValProgram)
%
%   Checks the labels in the program Program, producing ValProgram.
%   ValProgram is identical to Program except:
%   * There are no duplicate labels.
%   * All gotos point to a valid label.
%   * Labels have a unique global name.
%
%   @throws undefined_label(Name) if a label is used without being defined.
%   @throws duplicated_label(Name) if the same label is used more than once in the same function.

label_program(program(FunDecls), program(ValFunDecls)) :-
    maplist(label_function, FunDecls, ValFunDecls).

label_function(fun_decl(Name, Params, Body), fun_decl(Name, Params, ValBody)) :-
    (   Body = none
    ->  ValBody = none
    ;   empty_assoc(S0),
        label_block(Body, ValBody, S0, S),
        (   gen_assoc(L, S, entry(_, goto))
        ->  throw(undefined_label(L))
        ;   true
        )
    ).
   
label_block(block(Items), block(ValItems), S0, S) :-
    foldl(label_item, Items, ValItems, S0, S).

label_item(d(Decl), d(Decl), S, S).
label_item(s(Stmt), s(ValStmt), S0, S) :-
    label_stmt(Stmt, ValStmt, S0, S).

label_stmt(return(Exp), return(Exp), S, S).
label_stmt(expression(Exp), expression(Exp), S, S).
label_stmt(if(Cond, Then, Else), if(Cond, ValThen, ValElse), S0, S) :-
    label_stmt(Then, ValThen, S0, S1),
    (   Else = none
    ->  S = S1,
        ValElse = none 
    ;   label_stmt(Else, ValElse, S1, S)
    ).
label_stmt(compound(Block), compound(ValBlock), S0, S) :-
    label_block(Block, ValBlock, S0, S).
label_stmt(break, break, S, S).
label_stmt(continue, continue, S, S).
label_stmt(while(Exp, Stmt), while(Exp, ValStmt), S0, S) :-
    label_stmt(Stmt, ValStmt, S0, S).
label_stmt(do_while(Stmt, Exp), do_while(ValStmt, Exp), S0, S) :-
    label_stmt(Stmt, ValStmt, S0, S).    
label_stmt(for(Init, Cond, Post, Stmt), for(Init, Cond, Post, ValStmt), S0, S) :-
    label_stmt(Stmt, ValStmt, S0, S).
label_stmt(switch(Exp, Stmt), switch(Exp, ValStmt), S0, S) :-
    label_stmt(Stmt, ValStmt, S0, S).
label_stmt(case(Exp, Stmt), case(Exp, ValStmt), S0, S) :-
    label_stmt(Stmt, ValStmt, S0, S).
label_stmt(default(Stmt), default(ValStmt), S0, S) :-
    label_stmt(Stmt, ValStmt, S0, S).
label_stmt(goto(Label), goto(UniqueName), S0, S) :-
    (   get_assoc(Label, S0, entry(UniqueName, _))
    ->  S = S0
    ;   mk_labelname(Label, UniqueName),
        put_assoc(Label, S0, entry(UniqueName, goto), S)
    ).
label_stmt(labelled(Label, Stmt), labelled(UniqueName, ValStmt), S0, S) :-
    (   get_assoc(Label, S0, entry(UniqueName, How))
    ->  (   How = label
        ->  throw(duplicated_label(Label))
        ;   put_assoc(Label, S0, entry(UniqueName, label), S1) % How = goto
        )
    ;   mk_labelname(Label, UniqueName),
        put_assoc(Label, S0, entry(UniqueName, label), S1)
    ),
    label_stmt(Stmt, ValStmt, S1, S).
label_stmt(null, null, S, S).

%!  mk_labelname(+LabelName, -UniqueName)
%
%   Generate a unique name for the given label.

mk_labelname(Name, UniqueName) :-
    gensym('label.', Unique),
    atom_concat('label.', Id, Unique),
    atomic_list_concat(['label', Name, Id], '.', UniqueName).

%--------------------%
%   LOOP LABELLING   %
%--------------------%

%!  loop_program(+Program, -ValProgram)
%
%   @throws break_outside_loop if ... TBW ...
%   @throws continue_outside_loop if ... TBW ...
%   @throws case_without_switch if ... TBW ...
%   @throws default_without_switch if ... TBW ...

loop_program(program(FunDecls), program(ValFunDecls)) :-
    reset_gensym,
    maplist(loop_function, FunDecls, ValFunDecls).

loop_function(fun_decl(Name, Params, Body), fun_decl(Name, Params, ValBody)) :-
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

%!  gather_program(+Program, -ValProgram)
%
%   @throws non_constant_case if ... TBW ...
%   @throws duplicate_switch_clause(X) if ... TBW ...

gather_program(program(FunDecls), program(ValFunDecls)) :-
    maplist(gather_function, FunDecls, ValFunDecls).

gather_function(fun_decl(Name, Params, Body), fun_decl(Name, Params, ValBody)) :-
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

test(validate_goto) :-
    ProgramIn = program([
        fun_decl(fun1, [], block([
            s(goto(xxx)),
            s(return(constant(1)))
        ]))
    ]),
    catch(validate(ProgramIn, _), undefined_label(xxx), true).

test(validate_label) :-
    ProgramIn = program([
        fun_decl(fun1, [], block([
            s(goto(xxx)),
            s(labelled(xxx, null)),
            s(return(constant(1)))
        ])),
        fun_decl(fun2, [], block([
            s(labelled(xxx, null)),
            s(return(constant(1)))
        ]))
    ]),
    validate(ProgramIn, ProgramOut),
    assertion(is_valid_ast(ProgramOut)),
    assertion(ProgramOut = program([
        fun_decl(fun1, [], block([
            s(goto('label.xxx.1')),
            s(labelled('label.xxx.1', null)),
            s(return(constant(1)))
        ])),
        fun_decl(fun2, [], block([
            s(labelled('label.xxx.2', null)),
            s(return(constant(1)))
        ]))
    ])).

:- end_tests(semantics).