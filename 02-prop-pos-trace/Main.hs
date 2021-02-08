module Main where

import           Control.Monad (unless, when)
import           Data.Char
import           Data.IORef
import           GHC.IO        (unsafePerformIO)
import           Prelude       hiding (and, init, or, parse)
import           System.Exit   (exitFailure, exitSuccess)
import           System.IO

------------------------------------------------------------------------------
-- syntax
------------------------------------------------------------------------------

data Tm
  = And Tm Tm -- {t u}
  | Or Tm Tm -- [t u]
  | Name String -- t, {t}, [t]
  | Top -- {}
  | Bottom -- []
  deriving Eq

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
--   Rule application must be (must have been) confluent. (It is not confluent now.)
--   The left hand side is assumed to be a normal form.

type Env = Tm
type Val = Tm

-- initial: (Top | Bottom) -> Env -> (problem: Tm) -> Val
reduceDual :: Val -> Val -> Env -> Tm -> IO Val
reduceDual initial terminal = reduce where
  reduce e p = rewriteRec "reduce" reduce' e p where
    reduce' a (And b c)         = do l <- reduce a b; r <- reduce a c; and l r  -- R∧
    reduce' (And a b) c         = do l <- reduce a c; r <- reduce b c; or l r   -- L∧
    reduce' (Or a b) c          = do l <- reduce a c; r <- reduce b c; and l r  -- L∨
    reduce' a (Or b c)          = do l <- reduce a b; r <- reduce a c; or l r   -- R∨
    reduce' a b | a == b        = opCase "match"   initial  -- axiom
    reduce' a b | a == terminal = opCase "poe"     initial  -- principle of explosion
    reduce' _ b                 = opCase "default" b  -- return the problem unreduced

-- Normalize the conjunction of two logical terms.

-- Val -> Val -> Val     -- arguments must be normalized
and :: Val -> Val -> IO Val
and a b =
  do
    rewriteStep "and" and' a b
  where
    and' Top b = return b
    and' a Top = return a
    and' Bottom _ = return Bottom
    and' _ Bottom = return Bottom
    and' a b | a == b = return a
    and' a b = return (And a b) -- defer normalization

-- Normalize the disjunction of two logical terms.

-- Val -> Val -> Val      -- arguments must be normalized
or :: Val -> Val -> IO Val
or a b =
  do
    rewriteStep "or" or' a b
  where
    or' Bottom b = return b
    or' a Bottom = return a
    or' Top _ = return Top
    or' _ Top = return Top
    or' a b | a == b = return a
    or' a b = return (Or a b)

------------------------------------------------------------------------------
-- interpreters
------------------------------------------------------------------------------

-- Generic signature for interpreters.
data Ip env tm val = Ip
  { initial  :: env,
    eval     :: env -> tm -> IO val,
    extend   :: env -> val -> IO env,
    termd    :: env -> Bool, -- termination check
    quote    :: env -> val -> IO tm,
    quoteEnv :: env -> IO tm -- This function is intended to 'list' the environment.
    -- It makes most sense when env = val
    -- or maybe env = prg or env = ctx.
  }

-- A normalizing logic interpreter.
-- Note: we entirely stay in the syntax domain here, but the distinction
-- between environments, terms and values still makes sense.
-- For example, extending the environment with an unevaluated term
-- might break some properties of interpretation.
logicIp :: Val -> Val -> (Env -> Tm -> IO Val) -> Ip Env Tm Val
logicIp initial terminal append =
  Ip
    { initial = initial,
      eval = \env tm -> do
        rewriteRec "eval" reduce env tm
        ,
      extend = \env val -> do
        env' <- rewriteRec "backprop" reduce val env
        rewriteRec "append" append env' val
        ,
      termd = (== terminal),
      quote = const return,
      quoteEnv = return
    }
  where
    reduce = reduceDual initial terminal

-- Concrete logical interpreters.
conjIp = logicIp Top Bottom and
disjIp = logicIp Bottom Top or

-- Interpret terms in a void domain. Never terminate.
voidIp =
  Ip
    { initial = (),
      eval = \_ _ -> return (),
      extend = \_ _ -> return (),
      termd = const False,
      quote = const return,
      quoteEnv = return
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
    let (str, cs') = span isAlphaNum cs
     in TName (c : str) : tokenize cs'
  | c `elem` "{}[]" = TDelim c : tokenize cs
  | c `elem` " \t\r\n" = tokenize cs
  | otherwise = error $ "Unexpected character " ++ [c]

------------------------------------------------------------------------------
-- parser
------------------------------------------------------------------------------

-- Parsers consume the prefix of a token list and return the parsed term
-- together with the suffix of the list.

-- Generic term parser.
parse ::
  forall env tm val.
  (String -> tm) -> -- translate a name
  Ip env tm val -> -- interpret terms between {...}
  Ip env tm val -> -- interpret terms between [...]
  [Token] ->
  IO (tm, [Token])
parse parseName bracesIp bracketsIp = parseTerm
  where
    parseTerm :: [Token] -> IO (tm, [Token])
    parseTerm input = case input of
      []                -> error "unexpected end of input"
      TName str : rest  -> return (parseName str, rest)
      TDelim '{' : rest ->
        block "{...}" $ parseSeq '}' bracesIp (initial bracesIp) rest
      TDelim '[' : rest ->
        block "[...]" $ parseSeq ']' bracketsIp (initial bracketsIp) rest
      TDelim d : _      -> error $ "Unexpected delimiter " ++ [d]

    -- Parse a sequence of terms and interpret them in a nested environment.
    -- Note: shortcut semantics and skipTerm allow for single pass interpretation.
    parseSeq :: Char -> Ip env tm val -> env -> [Token] -> IO (tm, [Token])
    parseSeq eos ip env input = case input of
      [] -> error $ "expected " ++ [eos] ++ " but got end of input"
      TDelim d : rest | d == eos -> do qe <- quoteEnv ip env; return (qe, rest)
      _ ->
        if termd ip env
          then-- shortcut, skip all terms until the end of sequence
            do
              (_, rest) <- skipTerm input
              parseSeq eos ip env rest
          else -- parse and interpret the next term
            do
              (t, rest) <- parseTerm input
              val <- eval ip env t
              newEnv <- extend ip env val
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
printEnv Top       = return () -- don't list an empty program
printEnv (And a b) = do printEnv a; printEnv b
printEnv t         = do printTm t; newLine

-- Print a term using binary operators and full parenthesizing.
printBin :: Tm -> IO ()
printBin (And a b) = do putChar '('; printBin a; putStr " ∧ "; printBin b; putChar ')'
printBin (Or a b)  = do putChar '('; printBin a; putStr " ∨ "; printBin b; putChar ')'
printBin (Name n)  = do putStr n
printBin Top       = do putStr "⊤"
printBin Bottom    = do putStr "⊥"

------------------------------------------------------------------------------
-- debug tracing
------------------------------------------------------------------------------

-- ugly global variables

{-# NOINLINE tracing #-}
tracing :: IORef Int
tracing = unsafePerformIO $ newIORef 0

startTrace = do
  atomicModifyIORef tracing (\d -> (d + 1, ()))

stopTrace = do
  atomicModifyIORef tracing (\d -> (d - 1, ()))

{-# NOINLINE depth #-}
depth :: IORef Int
depth = unsafePerformIO $ newIORef 0

indent = do
  atomicModifyIORef depth (\d -> (d + 1, ()))

unindent = do
  atomicModifyIORef depth (\d -> (d - 1, ()))

prefix = do
  d <- readIORef depth
  putStr $ replicate (2 * d) ' '

space = putChar ' '

printDbg = printBin

rewriteStep name op a b = do
  dbg <- readIORef tracing
  if dbg > 0
    then do
      indent
      prefix; putStr name; space; printDbg a; space; printDbg b
      r <- op a b
      putStr " = "; printDbg r; newLine
      unindent
      return r
    else
      op a b

rewriteRec name op a b = do
  dbg <- readIORef tracing
  if dbg > 0
    then do
      indent
      prefix; putStr name; space; printDbg a; space; printDbg b; newLine
      indent
      r <- op a b
      unindent
      prefix; putStr "= "; printDbg r; newLine
      unindent
      return r
    else
      op a b

opCase name r = do
  dbg <- readIORef tracing
  when (dbg > 0) $ do prefix; putStr name; newLine
  return r

block name s = do
  dbg <- readIORef tracing
  if dbg > 0
    then do
      indent
      prefix; putStr "enter "; putStr name; newLine
      indent
      (tm, tokens) <- s
      unindent
      prefix; putStr "leave "; putStr name; newLine
      unindent
      return (tm, tokens)
    else
      s

------------------------------------------------------------------------------
-- main program
------------------------------------------------------------------------------

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

newLine = putStrLn ""

-- Detect if this is an interactive session.
{-# NOINLINE isTTY #-}
isTTY = unsafePerformIO $ hIsTerminalDevice stdin

-- The top level interpreter is implicitly conjunctive.
ip = conjIp

-- Interpret a term in the top level environment.
-- Note: we are doing essentially the same thing as parseSeq does,
-- but the loop is interactive here.
interpret :: Env -> String -> IO Env
interpret env str =
  do
    let input = tokenize str
    (t, _) <- parseTm input
    val <- eval ip env t
    nf <- quote ip env val
    newEnv <- extend ip env val
    when isTTY $ do
      printTm nf
      newLine
    return newEnv

-- Parse and evaluate a term in the top level environment.
evaluate :: Env -> [Token] -> IO ()
evaluate env input =
  do
    (t, _) <- parseTm input
    val <- eval ip env t
    nf <- quote ip env val
    printTm nf
    newLine

-- Fail if a term doesn't evaluate to true.
assertTheorem :: Env -> [Token] -> IO Env
assertTheorem env input =
  do
    (t, _) <- parseTm input
    val <- eval ip env t
    nf <- quote ip env val
    if nf == Top
      then
        return env
      else do
        putStr "can't prove "; printTm t; newLine
        return Bottom

-- Fail if two terms are not equal.
assertEq :: Env -> [Token] -> IO Env
assertEq env input =
  do
    (a, rest) <- parseTm input
    (b, _) <- parseTm rest
    vala <- eval ip env a
    valb <- eval ip env b
    nfa <- quote ip env vala
    nfb <- quote ip env valb
    dab <- eval ip vala nfb
    dba <- eval ip valb nfa
    if (dab == initial ip) && (dba == initial ip)
      then
        return env
      else do
        printTm nfa; putStr " /= "; printTm nfb; newLine
        return Bottom

-- Reduces a term in another term and extends the environment with the result.
reduceCmd :: Env -> [Token] -> IO Env
reduceCmd env input =
  do
    (a, rest) <- parseTm input
    (b, _) <- parseTm rest
    val <- eval ip a b
    newEnv <- extend ip env val
    nf <- quote ip env val
    when isTTY $ do
      printTm nf
      newLine
    return newEnv

-- Command processor.
command :: Env -> String -> IO Env
command env str =
  let input = tokenize str
   in case input of
        [] -> return env
        TName "l" : _ -> do qe <- quoteEnv ip env; printEnv qe; return env
        TName "c" : _ -> do
          when isTTY $
            putStrLn "#-------- clear --------"
          return (initial ip)
        TName "x" : _ -> do exitSuccess
        TName "h" : _ -> do help; return env
        TName "e" : rest -> do evaluate env rest; return env
        TName "th" : rest -> do assertTheorem env rest
        TName "eq" : rest -> do assertEq env rest
        TName "re" : rest -> do reduceCmd env rest
        TName "t1" : rest -> do startTrace; return env
        TName "t0" : rest -> do stopTrace; return env
        _ -> do putStrLn "unknown command"; return env

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
  putStrLn "  .re t u   - reduce u in t and extend the env. with the result"
  putStrLn "  .t1       - turn on debug trace"
  putStrLn "  .t0       - turn off debug trace"

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
      [] -> repl env
      '#' : _ -> repl env
      '.' : 't' : ' ' : rest -> do putStr "test "; putStrLn rest; repl (initial ip)
      '.' : rest -> do
        newEnv <- command env (trim rest)
        repl newEnv
      str -> do
        newEnv <- interpret env str
        repl newEnv

main :: IO ()
main = do
  when isTTY $ putStrLn "Type .h for help"
  repl (initial ip)
