:- use_module(library(main)).
:- use_module(lexer).
:- use_module(parser).
:- use_module(codegen).
:- use_module(emit).
:- initialization(main, main).

main(Argv) :-
    Argv = [File|_],
    writeln(File),
    read_file_to_string(File, Chars, []),
    scan(Chars, Tokens),
    parse(Tokens, Ast),
    generate(Ast, Asm),
    emit(Asm, Text),
    writeln(Text).
