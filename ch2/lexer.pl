/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(lexer, [scan/2]).
:- use_module(dcg_utils).

/** <module> Lexer
 
Lexer for Chapter 2 of "Writing a C Compiler".

The lexer parses strings into tokens.
A token is one of:
    | identifier(atom)
    | const(int)
    | return | int | void
    | '(' | ')' | '{' | '}' | ';' | '--' | '~' | '-'
*/

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
    ( no_digit | call(eos) ),   % 123abc is an error.
    { number_codes(I, Ds)
    }.

%!  no_digit
%
%   Peeks the next char and throws if it is the beginning of an identifier.
%   @throws invalid_digit(C) if the next char is the beginning of an identifier.

no_digit(), [X] -->
    [X],
    {   code_type(X, csymf)
    ->  throw(invalid_digit(X))
    ;   true
    }.

eos([], []).

digit(C) -->
    [C],
    { code_type(C, digit)
    }.

punctuator('--') -->
    "--".
punctuator(P) -->
    [C],
    { punctuator_char(C),
      atom_codes(P, [C])
    }.

punctuator_char(0'().
punctuator_char(0')).
punctuator_char(0'{).
punctuator_char(0'}).
punctuator_char(0'~).
punctuator_char(0'-).
punctuator_char(0';).

white_space(white_space(S)) -->
    [S],
    { code_type(S, space)
    }.

white_spaces() -->
    sequence(white_space, _).

xtoken(T) -->
    ( token(T)
    ; invalid_char(T)
    ),
    white_spaces().

invalid_char(C) -->
    [C],
    { throw(invalid_char(C))
    }.

xtokens(Ts) -->
    white_spaces(),
    sequence(xtoken, Ts).

%!  scan(+Source:string, -Tokens:[token]) is det.
%
%   Parses the source string into its constituent tokens.
%   @throws invalid_char(C) if an unknown char is present in the string.
%   @throws invalid_digit(C) if a number ends at the start of an identifier.

scan(Source, Tokens) :-
    string_codes(Source, Codes),
    once(phrase(xtokens(Tokens), Codes)).

:- begin_tests(lexer).

test(scan_identifiers) :-
    scan("abc return int void", X),
    X = [identifier('abc'), return, int, void].

test(scan_constants) :-
    scan("123 456", X),
    X = [constant(123), constant(456)].

test(scan_end) :-
    scan("123 ", X),
    X = [constant(123)].

test(scan_mixed) :-
    catch(scan("123abc", _), invalid_digit(C), true),
    C = 0'a.

test(scan_punctuators) :-
    scan("(){};--~-", X),
    X = ['(', ')', '{', '}', ';', '--', '~', '-'].

test(invalid) :-
    catch(scan("?", _), invalid_char(C), true),
    C = 0'?.

:- end_tests(lexer).

