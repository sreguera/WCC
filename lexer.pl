:- module(lexer, [scan/2]).
:- use_module(dcg_utils).

token(T) -->
    ( identifier(T)
    ; constant(T)
    ; punctuator(T)
    ).

keyword(int).
keyword(void).
keyword(return).

identifier(I) -->
    identifier_start(C0),
    sequence(identifier_continue, Cs),
    { atom_codes(A, [C0|Cs]),
      (  keyword(A)
      -> I = A
      ;  I = identifier(A)
      )
    }.

identifier_start(C) -->
    [C],
    { code_type(C, csymf)
    }.

identifier_continue(C) -->
    [C],
    { code_type(C, csym)
    }.

constant(constant(I)) -->
    sequence1(digit, Ds),
    { number_codes(I, Ds)
    }.

digit(C) -->
    [C],
    { code_type(C, digit)
    }.

punctuator(P) -->
    [C],
    { punctuator_char(C),
      atom_codes(P, [C])
    }.

punctuator_char(0'().
punctuator_char(0')).
punctuator_char(0'{).
punctuator_char(0'}).
punctuator_char(0';).

white_space(white_space(S)) -->
    [S],
    { code_type(S, space)
    }.

xtoken(T) -->
    ( token(T)
    ; white_space(_),
      token(T)
    ; invalid_char(T)
    ).

invalid_char(C) -->
    [C],
    { throw(invalid_char(C))
    }.

xtokens(Ts) -->
    sequence(xtoken, Ts).

scan(G, Ts) :-
    atom_codes(G, Cs),
    once(phrase(xtokens(Ts), Cs)).

:- begin_tests(lexer).

test(scan_identifiers) :-
    scan("abc return int void", X),
    X = [identifier('abc'), return, int, void].

test(scan_constants) :-
    scan("123", X),
    X = [constant(123)].

test(scan_punctuators) :-
    scan("(){};", X),
    X = ['(', ')', '{', '}', ';'].

test(invalid) :-
    catch(scan("?", _), invalid_char(C), true),
    C = 0'?.

:- end_tests(lexer).

