/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(lexer, [lex/2]).
:- use_module(dcg_utils).

/** <module> Lexer
 
Lexer for Chapter 2 of "Writing a C Compiler".

The lexer parses strings into tokens.
A token is one of:
    | identifier(atom)
    | constant(int)
    | return | int | void
    | '(' | ')' | '{' | '}' | ';' | '--' | '~' | '-'
*/

token(T) -->
    (   identifier(T)
    ;   constant(T)
    ;   punctuator(T)
    ).

keyword(int).
keyword(void).
keyword(return).

identifier(I) -->
    identifier_start(C0),
    sequence(identifier_continue, Cs),
    {   atom_codes(A, [C0|Cs]),
        (   keyword(A)
        ->  I = A
        ;   I = identifier(A)
        )
    }.

identifier_start(C) -->
    [C],
    {   code_type(C, csymf)
    }.

identifier_continue(C) -->
    [C],
    {   code_type(C, csym)
    }.

constant(constant(I)) -->
    sequence1(digit, Ds),
    ( no_digit | call(eos) ),   % 123abc is an error.
    {   number_codes(I, Ds)
    }.

%!  no_digit
%
%   Peeks the next char and throws if it is the beginning of an identifier.
%   @throws invalid_digit(C) if the next char is the beginning of an identifier.

no_digit(), [C] -->
    [C],
    {   code_type(C, csymf)
    ->  throw(invalid_digit(C))
    ;   true
    }.

eos([], []).

digit(C) -->
    [C],
    {   code_type(C, digit)
    }.

punctuator('--') -->
    "--".
punctuator(P) -->
    [C],
    {   punctuator_char(C),
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
    {   code_type(S, space)
    }.

white_spaces() -->
    sequence(white_space, _).

xtoken(T) -->
    (   token(T)
    ;   invalid_char(T)
    ),
    white_spaces().

invalid_char(C) -->
    [C],
    {   throw(invalid_char(C))
    }.

xtokens(Ts) -->
    white_spaces(),
    sequence(xtoken, Ts).

%!  lex(+Source:string, -Tokens:[token]) is det.
%
%   Parses the source string into its constituent tokens.
%   @throws invalid_char(C) if an unknown char is present in the string.
%   @throws invalid_digit(C) if a number ends at the start of an identifier.

lex(Source, Tokens) :-
    string_codes(Source, Codes),
    once(phrase(xtokens(Tokens), Codes)).


%-----------%
%   TESTS   %
%-----------%

:- begin_tests(lexer).

test(identifiers) :-
    lex("abc return int void", Ts),
    Ts = [identifier('abc'), return, int, void].

test(constants) :-
    lex("123 456", Ts),
    Ts = [constant(123), constant(456)].

test(white_space_at_end) :-
    lex("123 ", Ts),
    Ts = [constant(123)].

test(number_followed_by_letter_fails) :-
    catch(lex("123abc", _), invalid_digit(C), true),
    C = 0'a.

test(punctuators) :-
    lex("(){};--~-", Ts),
    Ts = ['(', ')', '{', '}', ';', '--', '~', '-'].

test(invalid_char_fails) :-
    catch(lex("?", _), invalid_char(C), true),
    C = 0'?.

:- end_tests(lexer).

