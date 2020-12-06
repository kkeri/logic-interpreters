# Problem reduction

*__Note__: the problem reducer is under development. Some complex cases
may not work as expected.*

The problem reduction algorithm is the heart of the logical interpreter.
It is implemented by the `reduce` operation.
Its type declares that both arguments and the return value are
propositional formulas.

~~~haskell
reduce :: Tm -> Tm -> Tm
~~~

`reduce e p` can be interpreted in several ways, but the overall idea
is solving a problem `p` in an environment `e`.
In contrast to [decision algorithms](https://en.wikipedia.org/wiki/Decision_problem)
that return yes or no, problem reduction returns the best available answer.

[Conditional theorem proof](https://en.wikipedia.org/wiki/Conditional_proof)
is a special case of problem reduction.
A conclusion `c` follows from an antecedent `a` iff `reduce a c = ⊤`.
Put differently, a proposition `p` is a theorem in environment `e`
iff `p` reduces to ⊤ in `e`.

## The `reduce` operation

The `reduce` function implements a variation of [sequent calculus](https://en.wikipedia.org/wiki/Sequent_calculus).
Let's break it down line by line.

The following four cases directly correspond to inference rules of
sequent calculus applied backwards to conjunction and disjunction.

~~~haskell
  reduce a (And b c)   = and (reduce a b) (reduce a c)    -- R∧
  reduce (And a b) c   = or  (reduce a c) (reduce b c)    -- L∧
  reduce a (Or b c)    = or  (reduce a b) (reduce a c)    -- R∨
  reduce (Or a b) c    = and (reduce a c) (reduce b c)    -- L∨
~~~

Looking at the rules one can notice a few differences compared to the
standard presentation of sequent calculus.

First, we can't find additional arguments Γ and Δ in the rules.
It is known that '*,*' on the left side of the turnstile should be thought of as
*and*, on the right side it should be thought of as *or*.
Having performed all possible conversions in advance,
we end up having a single formula both on the left side and on the right side.

Second, we can notice the use of the `and` and `or` operations.
They are responsible for computing the combination of subresults coming
from a split sequence.
In the standard formulation `and` is implied when multiple formulas are
written above the inference line, `or` is encoded in the proof procedure
by providing two variants of the same rule.
Their promotion first class operations allows us to ask questions
about them and also to change their implementation if needed.

The base case corresponds to the only axiom of sequent calculus.

~~~haskell
  reduce a b | a == b  = initial
~~~

This is the trivial case when an atom is derived from itself.
We don't have to deal with additional atoms on any side because the
four rules above pair up each atom of the antecedent with
each atom of the consequent.
The constant `initial` stands for ⊤ or ⊥ when `reduce` is called
from a conjunctive or disjuntive interpreter respectively.

Finally, there is a default case that applies when the right side
can't be derived from the left side. In this case the problem is returned
unchanged.

~~~haskell
  reduce _ b = b
~~~

To convert from problem reduction to simple decision, only the default case
has to be modified so that ⊥ is returned in a conjunctive interpreter
and ⊤ in a disjunctive interpreter.

### Properties

For every environment `e` and proposition `p`,
`reduce` satisfies the following equation:

`reduce (extend e (reduce e p)) p = initial`

It can be read as the solution of `p` gives us what is 'missing' from
the environment to make `p` a theorem.

[...]

## Further readings

[Interactive sequent calculus tutorial](http://logitext.mit.edu/tutorial)
