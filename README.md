# Jonah Sussman's - Language o' Things

## Running the interpreter

There are two ways to run the interpreter.

The first way is through the REPL. Simply execute `./lot` to bring it up.

The second way is to run a file. For instance, if you had a file named `examples/file.lot`, then in order to run it you would run `./lot examples/file.lot`.

## Compiling the interpreter

Simply run `make lot` from the terminal. If that does not work, the command `g++ lot -o lot` should work as well. 

NOTE: I only tested with `g++`. While there is no reason it shouldn't work on another compiler, please try and use this one.

## Project Structure

The documentation is in `presentation/presentation.html`. You will find everything you need inside that document.

`lot.cpp` contains the source code for the interpreter.

`examples/` contains the raw example code in the presentation.

`run_testcases.sh` will run the test cases in `examples`. It simply calls `./lot examples/<filename>` over a list of filenames that contain test cases.