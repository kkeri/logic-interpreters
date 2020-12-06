# Properties of interpretation

Let's recall steps of interpretation.

1. Read a term from the input.
2. Evaluate the term in the current environment.
4. If the value is a declaration (always), extend the environment with it.
3. Write the value to the output.

Steps 2 and 3 can be unified in an interpretation operation.
This page summarizes the properties of that operation and its dependencies. 
On this page `e` denotes environments, `t` and `u` denote input terms,
`Θ` denotes a sequence of input terms and `v` denote values.

~~~haskell
interpret :: Tm -> Tm -> Tm       -- implementation type
interpret e t = extend e (eval e t)
~~~

Interpretation is an idempotent operation.

`interpret e e = e`

Declaring a fact twice has the same effect as declaring it once.

`interpret (interpret e t) t = interpret e t`

### Evaluation

Defining the evaluation operation allows us to talk about the output
of the interpreter.

~~~haskell
eval :: Tm -> Tm -> Tm
eval e t = reduce e t
~~~

Note: `reduce` implements a problem reduction algorithm.
You can read more about it [here](reduce.md).
But I suggest you skim this page first.

Definitions:

A fact that evaluates to itself in `e` is a local axiom in `e`.  
A fact that evaluates to ⊤ in `e` is a theorem in `e`.  

All facts that have been declared in `e` are theorems in `e`.

`eval (interpret e t) t = ⊤`

~~~haskell
> a
a
> .l
a
> a
{}       -- '{}' represents ⊤
> .l
a
~~~

### Conjunction

Entering a conjunction into the interpreter has the same effect as
entering its elements one by one.

`interpret e {t u} = interpret (interpret e t) u`

According to the idempotence law, if a term `t` is declared before
declaring a conjunction that contains `t` as element, the value of the conjunction
doesn't contain `t`.

`eval (interpret e t) {t Θ} = {Θ}`

~~~haskell
> a
a
> {a b}
b
> .l
a
b
~~~

### Extension

The extension operation is defined as follows.

~~~haskell
extend :: Tm -> Tm -> Tm
extend e v = append (reduce v e) v
~~~

Notice how the `reduce` operation, the synonym of evaluation is
used 'backwards' here with the environment on the right side.
This can be read as the new declaration being backpropagated to the environment.
Finally the new declaration is appended to the new environment so
it may participate in the evaluation of subsequent terms.

### Termination

If a term evaluates to ⊥, the environment
collapses into ⊥ that is a terminal state of the interpreter.

`interpret e ⊥ = ⊥`

`interpret ⊥ t = ⊥`

~~~haskell
> []
[]
terminated> a
a
terminated> .l
[]
~~~

In interactive mode termination is indicated by the prompt,
giving the user chance to clear the environment.
In non-interactive mode the REPL exits immediately with an error code.

### Assertions

An assertion is a meta level command that computes a decision problem.
If the answer is *false*, the command terminates the interpreter
by extending the environment with ⊥, otherwise the environment is not changed.

### The top level environment

An *environment* is simply a term,
but we usually think of it as a sequence of declarations or a set of premises.
Until now I haven't defined the `append` operation.

~~~haskell
append :: Tm -> Tm -> Tm
append e v = and e v
~~~

The `and` operation computes the conjunction of two terms.
The initial environment is encoded by ⊤, the neutral element of conjunction.

`append ⊤ v = v`

`and t u` defaults to the conjunctive pair `{t u}`, so
eventually the top level environment is an *n*-ary conjunction of terms.
When listing the top level environment, each element of the
conjunction is printed in a separate line to support the intuition
that an environment is a sequence of declarations.

~~~haskell
> {a b c}
{a b c}
> .l
a
b
c
~~~

### Nested interpreters and duality

Up to this point we have been talking about the top level environment.
When the term parser encounters an opening brace `{`, a new
interpreter is created and it is sent all subsequent terms until
the closing `}` has been reached.
When the parser encounters the closing brace, the environment of
the nested interpreter is returned as a single term.

According to the duality of logical connectives (∧, ∨),
logic interpreters must also have their duals.
The dual of a conjunctive interpreter is a *disjunctive* interpreter,
where `append = or` and the initial environment is ⊥.
Disjunctive terms are interpreted by disjunctive interpreters.

In addition to `interpret`, the operations `eval`, `extend`, `append`
and `reduce` are also dual.
The dual of a theorem is a term that evaluates to ⊥ in a disjunctive
interpreter.

### Shortcut semantics

When an interpreter reaches a *terminal* state, interpretation of subsequent
terms doesn't alter the environment, but the interpreter still hangs up
if the evaluation of a term doesn't halt.
To prevent such a situation, many programming languages apply shortcut
semantics to logic expressions, where insignificant subexpressions
are not evaluated at all.

Logic interpreters support shortcut semantics by offering a
*termination check* predicate.
Termination check is utilized by the term parser which calls interpreters directly.

~~~haskell
termd :: Tm -> Bool
termd ⊥ = True
termd e = False
~~~

