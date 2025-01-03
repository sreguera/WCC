# Writing a C Compiler, a Prolog implementation

A Prolog implementation of the C compiler in the book "Writing a C Compiler" by Nora Sandler.

## System Requirements

* The ones in the book (Linux, MacOS or Windows with WSL, with gcc installed).
* [SWI-Prolog](https://www.swi-prolog.org/) 9.0.4+

## Usage

The compiler driver is the wcc file in each directory.

To run the book tests, checkout the book tests [repo](https://github.com/nlsandler/writing-a-c-compiler-tests) and then:

```bash
cd writing-a-c-compiler-tests
./test_compiler ../ch8/wcc --chapter 8  --extra-credit
```

## Chapters

Each directory contains the code for the corresponding chapter.

* ch1 - A Minimal Compiler
* ch2 - Unary Operators
* ch3 - Binary Operators
* ch4 - Logical and Relational Operators
* ch5 - Local Variables
* ch6 - if Statements and Conditional Expressions
* ch7 - Compound Statements
* ch8 - Loops
* ch9 - Functions
* ch10..ch20 - TODO

## Prolog and Compilers

Many Prolog books have a chapter or at least some examples on how to write a compiler,
usually for a small Pascal-like language.

* William F. Clocksin. *Clause and Effect*.
  Chapter 9 "Case Study: A Compiler for Three Model Computers".
* Leon Sterling and Ehud Shapiro. *The Art of Prolog*. [MIT Open Access](https://mitpress.mit.edu/9780262691635/the-art-of-prolog/).  
  Chapter 24 "A Compiler".
* Richard A. O'Keefe. *The Craft of Prolog*.
  In Section 1.5 "Difference Lists".

The cited paper in *Clause and Effect* and *The Craft of Prolog* is:

* David H. D. Warren. 1980. *Logic Programming and Compiler Writing*.
