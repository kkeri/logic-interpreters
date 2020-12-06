module Main where

import           Control.Monad (unless, when)
import           Data.Char
import           GHC.IO        (unsafePerformIO)
import           Prelude       hiding (and, init, or, parse)
import           System.Exit   (exitFailure, exitSuccess)
import           System.IO

------------------------------------------------------------------------------
-- syntax
------------------------------------------------------------------------------

data Tm
  = And Tm Tm        -- {t u}
  | Or Tm Tm         -- [t u]
  | Name String      -- t, {t}, [t]
  | Top              -- {}
  | Bottom           -- []

------------------------------------------------------------------------------
-- operations and predicates
------------------------------------------------------------------------------

-- This part is under development --

-- The reduceDual operation allows for sequent calculus style problem reduction.
-- Note: if we want to reduce problems to problems (formulas to formulas) rather
-- than yes/no, both 'reduce' and the subresult combinators 'and' and 'or'
-- have to return formulas in the default case.

-- Reduce the right hand side in the left hand side.
-- Requirements:
--   Rule application must be confluent.
--   The left hand side is assumed to be a normal form.
reduceDual :: Tm -> Tm -> Tm -> Tm
reduceDual initial = reduce where
  reduce a (And b c)   = and (reduce a b) (reduce a c)    -- R∧
  reduce (And a b) c   = or  (reduce a c) (reduce b c)    -- L∧
  reduce a (Or b c)    = or  (reduce a b) (reduce a c)    -- R∨
  reduce (Or a b) c    = and (reduce a c) (reduce b c)    -- L∨
  reduce a b | a == b  = initial                        -- axiom
  reduce _ b = b   -- return the problem unreduced


-- Normalize the conjunction of two logical terms.
and :: Tm -> Tm -> Tm
and Top b = b
and a Top = a
and Bottom _ = Bottom
and _ Bottom = Bottom
and a b | a == b = a
and a b = And a b  -- defer normalization


-- Normalize the disjunction of two logical terms.
or :: Tm -> Tm -> Tm
or Bottom b = b
or a Bottom = a
or Top _ = Top
or _ Top = Top
or a b | a == b = a
or a b = Or a b


-- Equality of normal forms.
instance Eq Tm where
  -- apply commutativity here, because normal forms are unordered
  (And a b) == (And a' b') = a == a' && b == b' || a == b' && b == a'
  (Or a b) == (Or a' b')   = a == a' && b == b' || a == b' && b == a'
  (Name s) == (Name t)     = s == t
  Top == Top               = True
  Bottom == Bottom         = True
  _ == _                   = False

------------------------------------------------------------------------------
-- interpreters
------------------------------------------------------------------------------

-- Generic signature for interpreters.
data Ip env tm val = Ip
  { initial  :: env,
    eval     :: env -> tm -> val,
    extend   :: env -> val -> env,
    termd    :: env -> Bool,  -- termination check
    quote    :: env -> val -> tm,
    quoteEnv :: env -> tm   -- This function is intended to 'list' the environment.
                            -- It makes most sense when env = val
                            -- or maybe env = prg or env = ctx.
  }

-- A normalizing logic interpreter.
-- Note: we entirely stay in the syntax domain here, but the distinction
-- between environments, terms and values still makes sense.
-- For example, extending the environment with an unevaluated term
-- might break some properties of interpretation.
logicIp :: Tm -> Tm -> (Tm -> Tm -> Tm) -> Ip Tm Tm Tm
logicIp initial terminal append = Ip
  { initial = initial,
    eval     = reduce,
    extend   = \env val -> append (reduce val env) val,
    termd    = (== terminal),
    quote    = const id,
    quoteEnv = id
  } where
    reduce = reduceDual initial

-- Concrete logical interpreters.
conjIp = logicIp Top Bottom and
disjIp = logicIp Bottom Top or

-- Interpret terms in a void domain. Never terminate.
voidIp = Ip
  { initial = (),
    eval     = \_ _ -> (),
    extend   = \_ _ -> (),
    termd    = const False,
    quote    = const id,
    quoteEnv = const ()
  }

------------------------------------------------------------------------------
-- tokenizer
------------------------------------------------------------------------------

data Token
  = TName String
  | TDelim Char

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | isAlpha c =
    let (str, cs') = span isAlpha cs in
    TName (c : str) : tokenize cs'
  | c `elem` "{}[]" = TDelim c : tokenize cs
  | c `elem` " \t\r\n" = tokenize cs
  | otherwise = error $ "Unexpected character " ++ [c]

------------------------------------------------------------------------------
-- parser
------------------------------------------------------------------------------

-- Parsers consume the prefix of a token list and return the parsed term
-- together with the suffix of the list.

-- Generic term parser.
parse :: forall env tm val.
  (String -> tm) ->        -- translate a name
  Ip env tm val ->         -- interpret terms between {...}
  Ip env tm val ->         -- interpret terms between [...]
  [Token] -> (tm, [Token])
parse parseName bracesIp bracketsIp = parseTerm where

  parseTerm :: [Token] -> (tm, [Token])
  parseTerm input = case input of
    []                -> error "unexpected end of input"
    TName str  : rest -> (parseName str, rest)
    TDelim '{' : rest -> parseSeq '}' bracesIp (initial bracesIp) rest
    TDelim '[' : rest -> parseSeq ']' bracketsIp (initial bracketsIp) rest
    TDelim d   : _    -> error $ "Unexpected delimiter " ++ [d]

  -- Parse a sequence of terms and interpret them in a nested environment.
  -- Note: shortcut semantics and skipTerm allow for single pass interpretation.
  parseSeq :: Char -> Ip env tm val -> env -> [Token] -> (tm, [Token])
  parseSeq eos ip env input = case input of
    [] -> error $ "expected " ++ [eos] ++ " but got end of input"
    TDelim d : rest | d == eos -> (quoteEnv ip env, rest)
    _ -> if termd ip env
          then -- shortcut, skip all terms until the end of sequence
            let (_, rest) = skipTerm input in
            parseSeq eos ip env rest
          else -- parse and interpret the next term
            let (t, rest)   = parseTerm input in
            let val         = eval ip env t in
            let newEnv      = extend ip env val in
            parseSeq eos ip newEnv rest

-- Parse a logical term.
parseTm = parse Name conjIp disjIp

-- Consume input without computation.
skipTerm = parse (const ()) voidIp voidIp

------------------------------------------------------------------------------
-- printer
------------------------------------------------------------------------------

-- Print a term in a neutral context.
printTm :: Tm -> IO ()
printTm (And a b) = do putChar '{'; printConj (And a b); putChar '}'
printTm (Or a b)  = do putChar '['; printDisj (Or a b); putChar ']'
printTm (Name n)  = do putStr n
printTm Top       = do putStr "{}"
printTm Bottom    = do putStr "[]"

-- Print a term in a conjunctive context.
printConj (And a b) = do printConj a; putChar ' '; printConj b
printConj t         = printTm t

-- Print a term in a disjunctive context.
printDisj (Or a b) = do printDisj a; putChar ' '; printDisj b
printDisj t        = printTm t

-- Print the top level environment.
printEnv :: Tm -> IO ()
printEnv Top       = return ()  -- don't list an empty program
printEnv (And a b) = do printEnv a; printEnv b
printEnv t         = do printTm t; newLine

------------------------------------------------------------------------------
-- main program
------------------------------------------------------------------------------

trim :: String -> String
trim = f . f where
  f = reverse . dropWhile isSpace

newLine = putStrLn ""

-- Detect if this is an interactive session.
{-# NOINLINE isTTY #-}
isTTY = unsafePerformIO $ hIsTerminalDevice stdin

type Env = Tm

-- The top level interpreter is implicitly conjunctive.
ip = conjIp

-- Interpret a term in the top level environment.
-- Note: we are doing essentially the same thing as parseSeq does,
-- but the loop is interactive here.
interpret :: Env -> String -> IO Env
interpret env str =
  let input       = tokenize str in
  let (t, _)      = parseTm input in
  let val         = eval ip env t in
  let nf          = quote ip env val in
  let newEnv      = extend ip env val in
  do
    when isTTY $ do
      printTm nf
      newLine
    return newEnv

-- Parse and evaluate a term in the top level environment.
evaluate :: Env -> [Token] -> IO ()
evaluate env input =
  let (t, _)      = parseTm input in
  let val         = eval ip env t in
  let nf          = quote ip env val in
  do
    printTm nf
    newLine

-- Fail if a term doesn't evaluate to true.
assertTheorem :: Env -> [Token] -> IO Env
assertTheorem env input =
  let (t, _)      = parseTm input in
  let val         = eval ip env t in
  let nf          = quote ip env val in
  if nf == Top
    then
      return env
    else do
      putStr "can't prove "; printTm t;
      newLine
      return Bottom

-- Fail if two terms are not equal.
assertEq :: Env -> [Token] -> IO Env
assertEq env input =
  let (a, rest)   = parseTm input in
  let (b, _)      = parseTm rest in
  let nfa         = quote ip env (eval ip env a) in
  let nfb         = quote ip env (eval ip env b) in
  if nfa == nfb
    then
      return env
    else do
      printTm nfa; putStr " /= "; printTm nfb
      newLine
      return Bottom

-- Reduces a term in another term and extends the environment with the result.
reduceCmd :: Env -> [Token] -> IO Env
reduceCmd env input =
  let (a, rest)   = parseTm input in
  let (b, _)      = parseTm rest in
  let val         = eval ip a b in
  let nf          = quote ip env val in
  let newEnv      = extend ip env val in
  do
    when isTTY $ do
      printTm nf
      newLine
    return newEnv

-- Command processor.
command :: Env -> String -> IO Env
command env str =
  let input = tokenize str in
  case input of
    []                  -> return env
    TName "l" : _       -> do printEnv (quoteEnv ip env); return env
    TName "c" : _       -> do when isTTY $
                                putStrLn "#-------- clear --------"
                              return (initial ip)
    TName "x" : _       -> do exitSuccess
    TName "h" : _       -> do help; return env
    TName "e" : rest    -> do evaluate env rest; return env
    TName "th" : rest   -> do assertTheorem env rest
    TName "eq" : rest   -> do assertEq env rest
    TName "re" : rest   -> do reduceCmd env rest
    _                   -> do putStrLn "unknown command"; return env

help :: IO ()
help = do
  putStrLn "  .l        - list the environment"
  putStrLn "  .c        - clear the environment"
  putStrLn "  .x        - exit read-eval-print loop"
  putStrLn ""
  putStrLn "  Test commands:"
  putStrLn "  .t <text> - clear the environment and print 'test <text>'"
  putStrLn "  .e t      - evaluate t without extending the environment"
  putStrLn "  .th t     - assert t is a theorem"
  putStrLn "  .eq t u   - assert equality of t and u"
  putStrLn "  .re t u   - reduce t in u and extend the env. with the result"

-- Read-eval-print-loop.
repl :: Env -> IO ()
repl env = do
  when (termd ip env && not isTTY) exitFailure
  when isTTY $ do
    when (termd ip env) $ putStr "terminated"
    putStr "> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    line <- getLine
    case trim line of
      []                -> repl env
      '#' : _           -> repl env
      '.' : 't' : ' ' : rest -> do putStr "test "; putStrLn rest; repl (initial ip)
      '.' : rest        -> do newEnv <- command env (trim rest)
                              repl newEnv
      str               -> do newEnv <- interpret env str
                              repl newEnv

main :: IO ()
main = do
  when isTTY $ putStrLn "Type .h for help"
  repl (initial ip)
