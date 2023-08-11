# Lambda expression compiler

This project is a mini-compiler written in Haskell for lambda expressions in the standard (i.e. not simplified) notation.
The program has two modes in which it can be run: _test mode_ and _compile mode_. In test mode, the compiler's functionality
can be checked against a number of public tests, available in the main module (`Main.hs`). In compile mode, the compiler can
be used for evaluating lambda expressions read from `stdin` (the results are displayed to `stdout`).

## The notation used for lambda expressions
As it is known, the lambda calculus has three constructors for lambda expressions: the **Term**, the **Abstraction** and the
**Application**.

For Term, the notation used is $x$.

For Abstraction, the notation used is $λx.e$, where $e$ is a lambda expression.

For Application, the syntax is $(e_{1}\ \ e_{2})$, where $e_{1}$ and $e_{2}$ are lambda expressions.

**No other simplifications have been made.**

_Examples of lambda expressions_:
- $λx.λy.(x\ \ y)$
- $((λx.(x\ \ y)\ \ z)\ \ λx.(x\ \ x))$

## Running the project
In order to run the compiler, the following commands can be used:
- test mode
    ```
    make build
    make run mode=test
    make clean # for cleaning up the executables generated
    ```
- compile mode
    ```
    make build
    make run mode=compile
    make clean # for cleaning up the executables generated
    ```
