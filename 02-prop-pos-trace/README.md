# 02-prop-pos-trace

Based on: [01-prop-pos](../01-prop-pos/README.md)

Contributions:
- The code is rewritten in the IO monad.
- Debug tracing is supported.
- Removed the ad hoc equality operator on terms.
- The third and fourth cases of `reduce` have been swapped.
  (I noticed that they are not confluent,
  similarly to the first and second cases.)
- Added rule for the [principle of explosion](https://en.wikipedia.org/wiki/Principle_of_explosion).
- Doc: interpretation is explained through examples with debug traces.

## Debug tracing

Debug trace applies to normal interpretation as well as to assertions.

~~~haskell
# New commands:

.t1        -- turn on debug trace
.t0        -- turn off debug trace
~~~


### Terms printed in debug traces

The interpreter has a sequential surface syntax but the AST is a binary tree.
The pretty printer flattens out the tree when restoring the surface syntax,
hiding associativity of AST nodes.


~~~haskell
{{a b} c}              -- input
And (And "a" "b") "c"  -- internal repr. (simplified)
{a b c}                -- pretty print

{a {b c}}              -- input
And "a" (And "b" "c")  -- internal repr.
{a b c}                -- pretty print
~~~

To improve on this shortcoming,
terms are printed in logical notation in debug traces.

~~~haskell
{{a b} c}              -- input
((a ∧ b) ∧ c)          -- debug print

{a {b c}}              -- input
(a ∧ (b ∧ c))          -- debug print

{}                     -- input
⊤                      -- debug print

[]                     -- input
⊥                      -- debug print
~~~

### How to read debug traces

Let's start with a simple case,
entering the atom `a` into an empty environment.

~~~haskell
> a
  eval ⊤ a
      reduce ⊤ a
        default
      = a
  = a
  backprop a ⊤
      reduce a ⊤
        default
      = ⊤
  = ⊤
  append ⊤ a
      and ⊤ a = a
  = a
a
~~~

Let's break it down.

The first section traces evaluation of the entered term that is
implemented by the `reduce` function.
(I suggest you open `Main.hs` where you can find the definition of `reduce`.)
The default case is triggered because no other matching case found.
(I streamlined the code a bit to make it more readable.)

~~~haskell
  reduce _ b = b  -- return the problem unreduced
~~~

It reads as `a` can't be reduced in the empty environment.
In other words `a` evaluates to itself, yielding the value `a`.

~~~haskell
  eval ⊤ a
      reduce ⊤ a
        default
      = a
  = a
~~~

Now the interpreter has to extend the environment with the new value.
The first step of extension is called *backpropagation*.
(Don't worry if it doesn't mean too much right now.
You can think of it as redundancy elimination.
I give a more specific example below.)
In this case backpropagation has no effect, the environment hasn't changed.

~~~haskell
  backprop a ⊤
      reduce a ⊤
        default
      = ⊤
  = ⊤
~~~

Finally, the new value is appended to the environment returned by the last step,
yielding the new environment, in which evaluation of the next term takes place.

In a conjunctive interpreter (as the top level one) this step is implemented
by the `and` function, which obeys the usual equations of Boolean conjunction.

~~~haskell
  append ⊤ a
      and ⊤ a = a
  = a
~~~

Finally the interpreter prints the new value to the output.
This step is not part of debug tracing, so the value is printed in sequential syntax.

Let's enter `a` again. It has to be a theorem this time,
triggering the `match` case during evaluation,
which corresponds to a successful derivation.
`initial` stands for `⊤` in a conjunctive interpreter.

~~~haskell
  reduce a b | a == b = initial         -- axiom
~~~

In general the environment doesn't change when a theorem has been interpreted.

~~~haskell
> a
  eval a a
      reduce a a
        match
      = ⊤
  = ⊤
  backprop ⊤ a
      reduce ⊤ a
        default
      = a
  = a
  append a ⊤
      and a ⊤ = a
  = a
{}
~~~

### Tracing nested interpreters

The debug trace below demonstrates how `{}` and `[]` blocks are interpreted.

When the user enters a block, the interpreter creates a nested interpreter
which is a [disjunctive interpreter](../01-prop-pos/doc/interpret.md#nested-interpreters-and-duality)
in this particular case.
Each term in the block is interpreted by the nested interpreter.
Notice how the environment of a disjunctive interpreter is initialized to `⊥`
and new values are appended by the `or` operation.

~~~haskell
> [a b]
  enter [...]
      eval ⊥ a
          reduce ⊥ a
            default
          = a
      = a
      backprop a ⊥
          reduce a ⊥
            default
          = ⊥
      = ⊥
      append ⊥ a
          or ⊥ a = a
      = a
      eval a b
          reduce a b
            default
          = b
      = b
      backprop b a
          reduce b a
            default
          = a
      = a
      append a b
          or a b = (a ∨ b)
      = (a ∨ b)
  leave [...]
~~~

When the closing `]` is encountered, the environment of
the nested interpreter is returned as the value of the block.
(You may have noticed that parsing and interpretation are interleaved.)

Control goes back to the top level interpreter which is a conjunctive one,
and the value of the block is interpreted as a single term.

> A *value* may be a *term* as well because both are represented
> by the same data type.
> However the two concepts are not generally interchangeable,
> their use is governed by [laws of interpretation](../01-prop-pos/doc/interpret.md).

~~~haskell
  eval ⊤ (a ∨ b)
      reduce ⊤ (a ∨ b)
          reduce ⊤ a
            default
          = a
          reduce ⊤ b
            default
          = b
          or a b = (a ∨ b)
      = (a ∨ b)
  = (a ∨ b)
  backprop (a ∨ b) ⊤
      reduce (a ∨ b) ⊤
          reduce a ⊤
            default
          = ⊤
          reduce b ⊤
            default
          = ⊤
          and ⊤ ⊤ = ⊤
      = ⊤
  = ⊤
  append ⊤ (a ∨ b)
      and ⊤ (a ∨ b) = (a ∨ b)
  = (a ∨ b)
[a b]
~~~

### Backpropagation

Finally an example that clarifies the role of backpropagation.

Without clearing the environment `[a b]`, let's enter the atom `a`.
The fourth case of the `reduce` function matches the arguments.

~~~haskell
  reduce (Or a b) c = 
    do
      l <- reduce a c
      r <- reduce b c
      and l r  -- L∨
~~~

The debug trace reflects the program code.
The algorithm branches on the environment and the target term is derived
from both branches.
To succeed, both branches have to succeed but only one of them does.
Finally the value `a` is returned unchanged, according to the fact that
`a` can't be derived from `[a b]`.

~~~haskell
> a
  eval (a ∨ b) a
      reduce (a ∨ b) a
          reduce a a
            match
          = ⊤
          reduce b a
            default
          = a
          and ⊤ a = a
      = a
  = a
~~~

Backpropagation calls `reduce` again but this time the arguments are swapped.
The purpose of this step is redundancy elimination.
`[a b]` can be derived from `a`
(in natural deduction, the corresponding rule is [disjunction introduction](https://en.wikipedia.org/wiki/Disjunction_introduction)).
Consequently, everything that can be derived from `[a b]`
can be derived from `a` as well, so `[a b]` is redundant.

~~~haskell
  backprop a (a ∨ b)
      reduce a (a ∨ b)
          reduce a a
            match
          = ⊤
          reduce a b
            default
          = b
          or ⊤ b = ⊤
      = ⊤
  = ⊤
~~~

`[a b]` has been removed from the environment.
To preserve the "conclusive power" of the interpreter
we have to append `a` to the environment.

~~~haskell
  append ⊤ a
      and ⊤ a = a
  = a
a
~~~

