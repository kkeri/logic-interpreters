# 03-variables

Based on: [02-prop-pos-trace](../02-prop-pos-trace/README.md)

Contribution:
- Attempt to add variable bindings to the interpreter.

## Analysis

Usually an environment is a list of bindings.
But it's different in logic interpreters.

**Environments = Logical formulas ( = Programs)**

Problems:
- How to interpret variables without breaking the correspondence?
- How to resolve variable names without a `lookup` function?

## Decisions

What is the meaning of a variable binding?

~~~haskell
def x y = subst y x -- substitute y for x
~~~

Why not `let x = y in z`?

- `let..in` implies a chain-like program structure
- A logical formula is a binary tree of logical operators  
  → `def`s are leaves of the tree

What is the scope of a variable?

- the rest of the program (imperative or monadic style), or
- the whole program (top level declarations in a functional program)

What to do if the user redeclares a name?

1. disallow redeclaration  
   → return `Bottom` to indicate error
2. shadow the previous declaration  
   → scope: the rest of the program
3. merge declarations  
   → scope: the whole program
4. allow only if identical

**There are real world examples to each one.**

I go for option 3, because
- It is commutative (faithful to Boolean operators)  
- Doesn't destroy logical monotonicity

**Meaning of a program is order independent.**

  → Set of declarations  
  → Like top level declarations in Java or Haskell

## Implementation

### Syntax

~~~haskell
data Tm = ...
        | Def String Tm  -- def n v
        | ...
~~~

### Rule 1

~~~haskell
reduce' (Def n v) (Name o) | n == o = return v -- variable resolution

> def a b
> a
b            -- ok
> .l
def a {}     -- stating a variable name evacuates the variable declaration
b            -- can I evaluate a variable without stating it?


> a
> def a b
> .l
b            -- declaration is backpropagated, that's ok
def a b

> def a b    -- b
> def a c    -- c
> .e a
[b c]        -- could be {a b} as well

> def a b    -- b
> def x y    -- a (default)
> .e a
[b a]        -- expected: b

> def a b    -- b
> def b c    -- a (default)
> .e a
[b a]        -- expected: c (failed to backpropagate declaration)
~~~

Problems:

- we get results from unrelated `def`s
- Chained declarations are incorrectly handled.

### Rule 2

~~~haskell
reduce' (Def n v) (Name o) | n == o = return v -- variable resolution
reduce' a (Def n v) = do w <- reduce a v; return (Def n w) -- reduce value of new variable

> def a b    -- b
> def b c    -- a (default)
> .e a
c
> .l
def a c      -- second declaration is backpropagated
def b c

> def a b    -- b
> def x y    -- a (default)
> .e a
[b a]        -- expected: b

> def c d
> def b c
> def a b
> .l
def c d
def b d
[def a b def a d]  -- expected: def a d
~~~

Problems persist.

## Conclusion

Variables don't behave as expected.

Before adding even more rules, the reason seems obvious.
Names have double role as logical atoms as well as variable names.
These roles are not clearly separated.

Expected behavior of logical atoms:
- A logical atom denotes an atomic statement
- Entering a statement has the effect of declaring it  
  → it gets appended to the environment
- Statements can be proven and contribute to proofs

Expected behavior of variable names:
- A variable name denotes a variable
- Entering a variable name evaluates it  
  → it shouldn't be appended to the environment
- Irrelevant declarations shouldn't contribute to the evaluation of
  a variable name

The two roles interfere.
