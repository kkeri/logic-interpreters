# 01-prop-pos

This project implements an interpreter for logical formulas in the
positive fragment (∧, ∨) of propositional logic.
It is also a starting point for upcoming projects.

## Getting started

After building the project,
start the read-eval-print loop with `stack run`.
Enter terms at the prompt and let the program interpret them.
During the session you can get help on commands by typing `.h`.

## Syntax

~~~haskell
-- terms

t, u       -- names (atoms)

{}         -- empty conjunction, also the Top element
{t}        -- the same as t
{t u...}   -- conjunction of multiple elements

[]         -- empty disjunction, also the Bottom element
[t]        -- the same as t
[t u...]   -- disjunction of multiple elements

-- essential commands

.l         -- list the environment
.c         -- clear the environment
.as t      -- assert a term
.eq t u    -- assert equality of two terms
~~~

## Interpretation

The interpreter maintains an environment, which is initially empty.
Once the user enters a term, the following steps are taken.

1. The term is evaluated in the environment.
2. The value of the term is printed to the output.
3. The environment is extended with the value.
4. A new prompt is displayed.

Example:

~~~haskell
> a      -- the user enters 'a'
a        -- 'a' evaluates to itself
> .l     -- list the environment
a        -- 'a' is appended to the environment
~~~

Why does it work like this? Two analogies may help to make sense of it.

The first one is the read-eval-print loop of programming languages like Lisp.
If one enters an S-expression in a Lisp interpreter, the expression is
evaluated and the result is printed out. If the expression happens to be
a definition, it also gets added to the environment.
When the user enters another term, the new definition may participate in its
evaluation.

~~~ lisp
> (setq x 3)  ; Lisp
3
> (+ x 1)
4
~~~

The other metaphor is that of mathematical reasoning. In an argument one
uses definitions, theorems and proofs. If we constrain ourselves to
propositional logic, proofs can be automated, so definitions and
theorems suffice.
What is the significance of each?
Definitions provide new facts that can't be proved from axioms.
The intended "use" of a definition is its inclusion in the set of premises,
what is the environment of reasoning.

So in both cases it is reasonable to add definitions to the environment.
But what about theorems? In a decidable system a theorem is a non-fact.
It doesn't encode any knowledge that couldn't have been derived
from premises.
Hence the intended use of theorems is *assertion*, or verification
of the user's assumptions.
If an assumption doesn't hold, the argument shouldn't go on.
The interpreter provides the `.as` command to assert that a proposition
is a theorem.

~~~haskell
> {a b}      -- add premise
{a b}        -- the premise evaluated to itself
> .l         -- check out the environment
a
b
> .as [a b]  -- verify that '[a b]' can be proved from a,b
>            -- the assertion is verified
> .as c      -- verify that 'c' holds
terminated>  -- verification failed this time
~~~

The default interpretation of a term is a definition.
Why is it good for us?

The main purpose of an interpreter is running programs.
Programmers know that their programs mainly consist of definitions, so
it seems to be a good default.
If a definition happens to be a theorem, it evaluates to a neutral
proposition that doesn't add anything to the environment.
If a definition leads to contradiction, the environment
collapses into `Bottom` that is a terminal state of the interpreter.

~~~haskell
> []         -- introduce Bottom intentionally
[]
terminated>
~~~

It is even possible to enter terms that fall in between definitions and theorems.

~~~haskell
> a
a
> {a b}      -- 'a' evaluates to Top, i.e. it is a theorem
b            -- 'b' evaluates to itself, i.e. it is a definition
~~~

## Implementation

In this project an *environment* is simply a syntax term,
but we usually think of it as a sequence of terms or a set of premises.
When an environment *e* gets extended, a new term *n* is appended to it using
the `and` operation which computes the conjunction of two terms.

`e' = and e n`

The operation defaults to the `e'={e n}` conjunctive pair, so
eventually an environment is an *n*-ary conjunction of terms.

*Evaluation* of a term *t* in environment *e* amounts to finding a minimal
proposition *v* that, together with the current set of premises *e*,
is sufficient to prove *t*.
Evaluation is implemented by the `solve` non-commutative operation.
You can read more about it in the *Problem reduction* section.

`v = solve e t`

When an environment *e* is *extended* with a term *v*, the term is not simply
appended to the environment, but also retroactively applied to it.

`e' = and (solve v e) v`

### Nested interpreters

Until now I've been talking about the top level interpreter which is a
*conjunctive* one: composition of environments is computed by the `and`
operator, which computes the conjunction of terms.
Consequently, the empty environment is represented by the `Top` term,
the neutral element of conjunction.

When the user enters the term `{a b c}`, the program creates a new
interpreter that interprets all terms between the braces.
When the parser encounters the closing `}`, the environment accumulated in
the nested interpreter is returned as a single term.

According to the duality of connectives  (∧, ∨),
logical interpreters must also have their duals.
There exist *disjunctive* interpreters, where composition is `or` and
the neutral element is `Bottom`. Terms like `[d e f]` are interpreted by
disjunctive interpreters.

### Termination and shortcut semantics

When `Bottom` is appended to a conjunctive environment (or `Top` to a
disjunctive one), the environment reaches a *terminal* state.
Interpretation of subsequent terms doesn't alter the environment,
but the interpreter still hangs up if the evaluation of a term doesn't halt.
To prevent such a situation, many programming languages applie shortcut
semantics to logical expressions, where insignificant subexpressions
are not evaluated at all.

Logical interpreters support shortcut semantics by offering a
*termination check* predicate.
Termination check is utilized by the parser which calls interpreters directly.

### Problem reduction

[...]

## Building and running

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

