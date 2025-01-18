/* Copyright 2025 José Sebastián Reguera Candal
*/
:- module(scoped_table, 
    [ empty_table/1,
      table_push_scope/2,
      table_add_entry/4,
      table_add_base_entry/4,
      table_replace_base/3,
      table_get_local_entry/3,
      table_get_global_entry/3
    ]).

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
