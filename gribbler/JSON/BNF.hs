-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNF.hs: Abstract Backus–Naur form implementation
-- Copyright (C) 2021-2023 LStandman

module JSON.BNF(
    Parser(..),
    Result(..),
    JSON.BNF.and,
    JSON.BNF.drop,
    JSON.BNF.null,
    JSON.BNF.or,
    err,
    eval_parser,
    except,
    expect,
    exec_parser,
    from_hit,
    miss,
    oom,
    rep,
    run_parser,
    zom,
    zoo)
  where

import Control.Monad
import GHC.Stack

infixl 1 `err`
infixl 1 `and`
infixl 1 `or`
infixl 1 `except`

data Result a =
    Hit a        |
    Miss         |
    Error String
  deriving (Eq, Show)

newtype Parser s a = Parser (s -> Result (a, s))

and         :: Semigroup a => Parser s a -> Parser s a -> Parser s a
drop        :: Monoid b => Parser s a -> Parser s b
eval_parser :: Parser s a -> s -> Result a
except      :: Parser s a -> Parser s a -> Parser s a
expect      :: String -> Parser s a -> Parser s a
exec_parser :: Parser s a -> s -> Result s
from_hit    :: HasCallStack => Result a -> a
null        :: Monoid a => Parser s a
oom         :: Semigroup a => Parser s a -> Parser s a
or          :: Parser s a -> Parser s a -> Parser s a
rep         :: Semigroup a => Int -> Parser s a -> Parser s a
run_parser  :: Parser s a -> (s -> Result (a, s))
zom         :: Monoid a => Parser s a -> Parser s a
zoo         :: Monoid a => Parser s a -> Parser s a

instance Monad Result
  where
    return = Hit
    (Hit h)   >>= f = f h
    Miss      >>= _ = Miss
    (Error e) >>= _ = Error e

instance Applicative Result
  where
    pure  = return
    (<*>) = ap

instance Functor Result
  where
    fmap = liftM

instance Monad (Parser s)
  where
    return x = Parser (\ s -> Hit (x, s))
    (Parser f') >>= g =
      Parser (\ s -> f' s >>= \ (x, s') -> run_parser (g x) s')

instance Applicative (Parser s)
  where
    pure  = return
    (<*>) = ap

instance Functor (Parser s)
  where
    fmap = liftM

run_parser (Parser f') = f'

from_hit (Error e) = error ("JSON.BNF.from_hit: Error <" ++ e ++ ">")
from_hit Miss      = error ("JSON.BNF.from_hit: Miss")
from_hit (Hit x)   = x

eval_parser f s = run_parser f s >>= return . fst

exec_parser f s = run_parser f s >>= return . snd

or (Parser f') g =
  Parser (\ s -> case f' s of
    Miss -> run_parser g s
    r    -> r)

and f g =
  f >>= \ x -> g >>= \ x' -> return (x <> x')

err :: String -> Parser s a
err e = Parser (\ _ -> Error e)

miss :: Parser s a
miss = Parser (\ _ -> Miss)

rep 1 f = f
rep n f = f `JSON.BNF.and` rep (n - 1) f

except f g =
  Parser (\ s -> run_parser 
    (f >>= \ x -> case run_parser g s of
      Hit   _ -> miss
      Miss    -> return x
      Error e -> err e) s)

expect e1 (Parser f') =
  Parser (\ s -> case f' s of
    Hit   x  -> return x
    Miss     -> Error e1
    Error e2 -> Error e2)

null = return mempty

zoo f = f `JSON.BNF.or` JSON.BNF.null

zom f = f `JSON.BNF.and` zom f `JSON.BNF.or` JSON.BNF.null

oom f = f `JSON.BNF.and` oom f `JSON.BNF.or` f

drop f = f >> JSON.BNF.null
