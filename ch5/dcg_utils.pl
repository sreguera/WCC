/* Copyright 2024 José Sebastián Reguera Candal
*
* Copyright and License of most of this file is the one in SWI-Prolog.
* See: https://github.com/SWI-Prolog/swipl-devel/blob/master/library/dcg/high_order.pl
*/
:- module(dcg_utils, [
    sequence/4,
    sequence1/4,
    optional/4
]).

:- meta_predicate 
    sequence1(3, ?, ?, ?),
    sequence(3, ?, ?, ?),
    optional(//, //, ?, ?).

sequence1(OnElem, [H|List]) -->
    call(OnElem, H),
    sequence_(List, OnElem).

sequence(OnElem, List) -->
    sequence_(List, OnElem).

sequence_([H|T], P) -->
    call(P, H),
    sequence_(T, P).
sequence_([], _) -->
    [].

optional(Match, _Default) -->
    Match, !.
optional(_, Default) -->
    Default, !.