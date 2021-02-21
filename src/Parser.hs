{-# LANGUAGE InstanceSigs #-}

module Parser where

import Control.Applicative

newtype Parser s a = Parser { runParser :: [s] -> [(a, [s])] }

item :: Parser s s
item = Parser $ \l -> case l of
  []       -> []
  (x : xs) -> [(x, xs)]

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser p) = Parser $ \l -> case p l of
    []        -> []
    [(a, l')] -> [(f a, l')] 

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $ \l -> [(x, l)]

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser pf <*> Parser pa = Parser $ \l -> case pf l of
    []        -> []
    [(f, l')] -> case pa l' of
      []         -> []
      [(a, l'')] -> [(f a, l'')]

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser p >>= f = Parser $ \l -> case p l of
    []        -> []
    [(a, l')] -> runParser (f a) l'

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ \_ -> []

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser x <|> Parser y = Parser $ \l -> case x l of
    []      -> y l
    [(a, l')] -> [(a, l')]

instance MonadFail (Parser s) where
  fail _ = empty

evalParser :: Parser s a -> [s] -> [a]
evalParser = (fmap fst .) . runParser

execParser :: Parser s a -> [s] -> [[s]]
execParser = (fmap snd .) . runParser


-----------------------
-- Basic combinators --
-----------------------


ok :: Parser s ()
ok = pure ()

eof :: Parser s ()
eof = Parser $ \l -> case l of
  [] -> [((), [])]
  _  -> []

satisfy :: (s -> Bool) -> Parser s s
satisfy predicate = Parser $ \l -> case l of
  []       -> []
  (x : xs) -> if predicate x then [(x, xs)] else []

element :: Eq s => s -> Parser s s
element c = satisfy (== c)
 
stream :: Eq s => [s] -> Parser s [s]
stream []       = return []
stream (x : xs) = element x >>= \x' -> stream xs >>= \xs' -> return (x' : xs')