module Token where

import Data.Char

data Token = LPar
           | RPar
           | Comma
           | Colon
           | Arrow
           | EqualsSign
           | DefKeyword
           | Identifier String
           | TypeHint String
           | HALT
           deriving (Eq, Ord, Show, Read)

tokenize :: String -> [Token]
tokenize = tokenize' False

tokenize' :: Bool -> String -> [Token]
tokenize' b []               = []
tokenize' b ('(' : xs)       = LPar : tokenize' b xs
tokenize' b (')' : xs)       = RPar : tokenize' b xs
tokenize' b (',' : xs)       = Comma : tokenize' b xs
tokenize' b (':' : xs)       = Colon : tokenize' True xs
tokenize' b ('-' : '>' : xs) = Arrow : tokenize' True xs
tokenize' b ('=' : xs)       = EqualsSign : tokenize' b xs
tokenize' b ('d' : 'e' : 'f' : x : xs)
  | isSpace x = DefKeyword : tokenize' b xs
tokenize' b s@(x : xs)
  | isAlpha x || x == '_' = if b 
                            then let (satisfied, rest) = span (\c -> isAlphaNum c || c == '_'
                                                                                  || c == ']'
                                                                                  || c == '[') s
                                 in TypeHint satisfied : tokenize' False rest
                            else let (satisfied, rest) = span (\c -> isAlphaNum c || c == '_') s
                                 in Identifier satisfied : tokenize' b rest
  | isSpace x             = tokenize' b xs
tokenize' _ _                = [HALT]