/* Copyright 2024 José Sebastián Reguera Candal
*/
:- use_module(library(main)).
:- use_module(lexer).
:- use_module(parser).
:- use_module(semantics).
:- use_module(tacky).
:- use_module(codegen).
:- use_module(emit).
:- initialization(main, main).

main(Argv) :-
    Argv = [Flag, IPath, SPath|_],
    read_file_to_string(IPath, Source, []),
    lex(Source, Tokens),
    (   Flag = '--lex'
    ->  true
    ;   parse(Tokens, Ast),
        (   Flag = '--parse'
        ->  true
        ;   validate(Ast, ValAst),
            (   Flag = '--validate'
            ->  true
            ;   tack(ValAst, Tacky),
                (   Flag = '--tacky'
                ->  true
                ;   generate(Tacky, Asm),
                    (   Flag = '--codegen'
                    ->  true
                    ;   emit(Asm, AsmText),
                        open(SPath, write, SStream),
                        with_output_to(SStream, write(AsmText))
                    )
                )
            )
        )
    ).
