:- module(dcg_utils, [
    sequence/4,
    sequence1/4,
    optional/4
]).

% See:
% https://github.com/SWI-Prolog/swipl-devel/blob/master/library/dcg/high_order.pl

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