# Jonah Sussman's 503 Lisp Interpreter

## Running the interpreter

There are two ways to run the interpreter.

The first way is through the REPL. Simply execute `./lot` to bring it up.

The second way is to run a file. For instance, if you had a file named `examples/0_test_cases.lot`, then in order to run it you would run `./lot 0_test_cases.lot`.

## Compiling the interpreter

Simply run `make lot` from the terminal. If that does not work, the command `g++ lot -o lot` should work as well. 

NOTE: I only tested with `g++`. While there is no reason it shouldn't work on another compiler, please try and use this one.

## Project Structure

The documentation is in `presentation/presentation.html`. You will find everything you need inside that document.

`lot.cpp` contains the source code for the interpreter.

`examples` contains the raw example code present in the presentation.