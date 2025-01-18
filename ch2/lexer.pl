/* Copyright 2024 José Sebastián Reguera Candal
*/
:- module(lexer,
    [ is_token/1,   % Succeeds if Token is one of the tokens generated by the lexer.
      lex/2         % Parses the Source string into its constituent Tokens.
    ]).
:- use_module(library(dcg/basics), [eos//0]).
:- use_module(library(dcg/high_order)).

/** <module> Lexer
 
Lexer for Chapter 2 of "Writing a C Compiler".

The lexer parses strings into tokens.

*/

%!  is_token(+Token)
%
%   Succeeds if Token is one of the tokens generated by the lexer.

is_token(identifier(Name)) :-
    atom(Name).
is_token(constant(Value)) :-
    integer(Value).
% Keywords
is_token(int).
is_token(return).
is_token(void).
% Punctuators
is_token('--').
is_token('-').
is_token(';').
is_token('(').
is_token(')').
is_token('{').
is_token('}').
is_token('~').


%!  lex(+Source:string, -Tokens:[token]) is det.
%
%   Parses the Source string into its constituent Tokens.
%   @throws invalid_char(C) if an unknown char is present in the string.
%   @throws invalid_digit(C) if a number ends at the start of an identifier.

lex(Source, Tokens) :-
    string_codes(Source, Codes),
    once(phrase(xtokens(Tokens), Codes)).

%!  xtokens(-Ts)//
%
%   Parses a sequence of tokens, checking for invalid chars and removing
%   white space.
%
%   @throws invalid_char(C) if an unknown char is present in the string.
%   @throws invalid_digit(C) if a number ends at the start of an identifier.

xtokens(Ts) -->
    white_spaces(),
    sequence(xtoken, Ts).

xtoken(T) -->
    (   token(T)
    ;   invalid_char(T)
    ),
    white_spaces().

invalid_char(C) -->
    [C],
    {   throw(invalid_char(C))
    }.

%!  token(-T)//
%
%   Parses one token from the input stream.

token(T) -->
    (   identifier(T)
    ;   constant(T)
    ;   punctuator(T)
    ).

%!  keyword(K)
%
%   K is a keyword.

keyword(int).
keyword(void).
keyword(return).

%!  identifier(-I)//
%
%   Parses an identifier. The identifier may be a keyword.

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

%!  constant(-C)//
%
%   Parses a constant.

constant(constant(I)) -->
    digit(D0),
    sequence(digit, Ds),
    ( no_digit | eos ),   % 123abc is an error.
    {   number_codes(I, [D0|Ds])
    }.

%!  no_digit//
%
%   Peeks the next char and throws if it is the beginning of an identifier.
%   @throws invalid_digit(C) if the next char is the beginning of an identifier.

no_digit(), [C] -->
    [C],
    {   code_type(C, csymf)
    ->  throw(invalid_digit(C))
    ;   true
    }.

digit(C) -->
    [C],
    {   code_type(C, digit)
    }.

%!  punctuator(-P)//
%
%   Parses a punctuator.

punctuator('(') -->  "(", !.
punctuator(')') -->  ")", !.
punctuator('{') -->  "{", !.
punctuator('}') -->  "}", !.
punctuator('~') -->  "~", !.
punctuator('--') -->  "--", !.
punctuator('-') -->  "-", !.
punctuator(';') -->  ";", !.

%!  white_space(-S)//
%
%   Parses white space.

white_space(white_space(S)) -->
    [S],
    {   code_type(S, space)
    }.

white_spaces() -->
    sequence(white_space, _).


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

