# 01-prop-pos

This project implements a logic formula interpreter in
the positive fragment of propositional logic.
Hopefully this is the first one in a series of prototypes where each one
will extend its predecessors or explore new directions. 

The main contribution of this initial project is asking the question:
what is a logic interpreter?
A successful answer creates a viable conceptual framework for upcoming projects
as well as an extendable code base.

## Motivation

Throughout a line of projects I'd like to develop an interactive
problem solving environment.
A problem can be either an abstract logic problem or an
everyday operative task.
Think of the usual activities of power users:
finding and organizing information, interacting with databases,
automating repetitive tasks, managing processes, communication etc.
I envision an environment where *doing* is almost the same as programming.

Surprisingly the environment which best approximates this idea is
the command line with shell scripting.
A possible reason is that shell scripting offers simple but powerful
abstractions (processes, input and output streams, pipes)
that support composition more than any kind of GUI.

The [CALM theorem](https://rise.cs.berkeley.edu/blog/an-overview-of-the-calm-theorem/)
(consistency as logical monotonicity)
calls for another promising use case: the implementation of distributed
cooperation networks.

## Getting started

Initialize the project with `stack setup`,
then start the read-eval-print loop with `stack run`.
Enter terms at the prompt and let the program interpret them for you.
You can also issue meta level commands (see below).

## Syntax

Propositional formulas are written in an alternative syntax
which I believe fits well with programming languages.
Instead of binary operators, logic connectives are expressed in
n-ary block format.

~~~haskell
-- terms

t, u       -- names (atoms)

{}         -- empty conjunction, also ⊤
{t}        -- the same as t
{t u...}   -- conjunction of multiple elements

[]         -- empty disjunction, also ⊥
[t]        -- the same as t
[t u...]   -- disjunction of multiple elements

-- essential commands

.l         -- list the environment
.c         -- clear the environment
.h         -- get help on all commands
.th t      -- assert t is a theorem
.eq t u    -- assert equality of two terms
~~~

## Interpretation

Let me remind you that my intention here is to place an extendable calculus
into a workflow familiar with programmers.
Having only a barebone calculus it's not easy to demonstrate,
so let me guide you
even if the purpose of everything is not obvious yet.

First, let's look at the interpreter as a read-eval-print loop.
It repeats three steps:

1. Reads a term from the input.
2. Evaluates the term.
3. Writes the value to the output.

Below is a short dialog with a Lisp REPL.
You can even try it out [online](http://lisperator.net/slip/) (click open demo).

~~~ lisp
> (setq x 3)  ; define the global variable 'x'
3
> (+ x 1)     ; read and evaluate 'x+1'
4             ; print its value
~~~

You may have noticed that something is missing from the three steps above.
Some terms, called *declarations* are special.
Those terms can alter the interpretation of subsequent terms.
To be able to remember them, the interpreter maintains an *environment*,
and every time it encounters a declaration, extends the
environment with it.

How can we adapt this pattern to our interpreter?
In the propositional calculus there are no variables, only atoms that stand
for themselves. So what is a declaration?
The radical answer: *every term is a declaration*, because
every term may alter the interpretation of subsequent terms.

## Examples

One way to think about the interpreter is a conditional theorem prover.

Let's introduce two premises and assert their conjunction is a theorem.

~~~haskell
> a
a
> b
b
> .th {a b}
>              -- empty output indicates success
~~~

If an assertion has failed, the interpreter terminates.
In interactive mode you can recover from termination by clearing
the environment.

~~~haskell
> [a b]
[a b]
> .th {a b}
terminated> .c -- 'terminated' indicates failure
> .l
>              -- fresh again!
~~~

Theorem proof is a [decision problem](https://en.wikipedia.org/wiki/Decision_problem),
but the real strength of the interpreter relies in its *problem reduction*
capabilities.
We can ask the interpreter 'what is missing' to make a term a theorem.
Eventually a theorem is just a term that evaluates to true:

~~~haskell
> {a b}
{a b}
> a
{}
~~~

The interpreter answered that 'nothing is missing' for `a` to be a theorem.
This is the friendly version of assertion.

Let's try something less obvious.
A disjunction can be interpreted as the exact list of terms that
make it a theorem:

~~~haskell
> [i j]
[i j]
> i       -- declare one of the alternatives
i
> [i j]
{}        -- it is a theorem now
> .l
i         -- but where is [i j]?
~~~

Listing the environment testifies that `[i j]` 'magically' disappeared.
The reason: when the environment had been extended with `i`,
the new declaration was *backpropagated* to the environment.
As `i` implies `[i j]`, the latter became redundant.
This is not only an optimization but an essential part of the semantics
of interpretation.

We can also think of the sequence of input terms as a *program*.
Eventually a program consists of declarations, let them be
types, variables, functions, imports etc., and optionally a main program.
(The [test script](test.in) of this project is written this way.)
This viewpoint is entirely compatible with the *theorem prover* viewpoint,
and it's a nice thing!
Of course this simple calculus is too skinny for a programming language
(we don't even have variables and functions) but don't forget,
this is just the beginning.

If you are interested,
you can learn about the details [here](doc/interpret.md).

## Building, running and testing

~~~sh
cd <project-dir>

# setting up the toolchain
stack setup

# building the executable
stack install

# running the REPL
stack run

# running test
stack run < test.in
~~~
