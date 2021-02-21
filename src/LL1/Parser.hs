{-# LANGUAGE TupleSections #-}

module LL1.Parser where

import Control.Applicative

data Parser s a = Parser { production :: Maybe a
                         , nextParser :: s -> Maybe (Parser s a)
                         }

runParser :: Parser s a -> [s] -> Maybe (a, [s])
runParser p []       = fmap (, []) $ production p
runParser p (x : xs) = case nextParser p x of
  Nothing -> case production p of
    Nothing -> Nothing
    Just a  -> Just (a, xs)
  Just nP -> runParser nP xs

instance Functor (Parser s) where
  fmap f ~(Parser ep np) = Parser (fmap f ep) $ fmap (fmap f) . np

instance Applicative (Parser s) where
  pure x = Parser (Just x) $ \_ -> Nothing
  Parser fep fnp <*> ~x@(Parser xep xnp) = Parser (fep <*> xep) $ \s -> case fnp s of
    Just fp -> Just (fp <*> x)
    Nothing -> case fep of
      Just fep' -> fmap (fmap fep') (xnp s)
      Nothing -> Nothing

instance Alternative (Parser s) where
  empty = Parser Nothing $ \_ -> Nothing
  Parser aep anp <|> ~(Parser bep bnp) = Parser (aep <|> bep) $ \s -> anp s <|> bnp s

evalParser :: Parser s a -> [s] -> Maybe a
evalParser = (fmap fst .) . runParser

execParser :: Parser s a -> [s] -> Maybe [s]
execParser = (fmap snd .) . runParser


-----------------------
-- Basic combinators --
-----------------------


satisfy :: (s -> Bool) -> Parser s s
satisfy predicate = Parser Nothing $ \s -> if predicate s then Just (pure s) else Nothing

element :: Eq s => s -> Parser s s
element c = satisfy (== c)

stream :: Eq s => [s] -> Parser s [s]
stream []       = pure []
stream (x : xs) = liftA2 (:) (element x) (stream xs)