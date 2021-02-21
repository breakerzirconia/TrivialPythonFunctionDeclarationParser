{-# LANGUAGE LambdaCase    #-}

module Combinators where

import Control.Applicative
import Data.Char
import Def
import Parser
import Token

{-

The context-free grammar for parsing Python function declarations:

S  ::= def N ( P ) R = {the main body of the function declaration}
N  ::= <identifier_name> T {id name can only be constructed via A-Za-z0-9_, cannot start with a digit}
T  ::= : T'
     | \EPSILON
T' ::= <type_hint> {same constraints as in <identifier_name>, but can also contain square brackets}
P  ::= N P' {listing the parameters, left-recursive parsing}
     | \EPSILON
P' ::= , N P'
     | \EPSILON
R  ::= -> T'
     | \EPSILON

-}

isTypeHint :: Parser Token Token
isTypeHint = satisfy $ \case
  TypeHint _ -> True
  _          -> False

isIdentifier :: Parser Token Token
isIdentifier = satisfy $ \case 
  Identifier _ -> True
  _            -> False

parseS :: Parser Token Def
parseS = do element DefKeyword
            Identifier functionName <- isIdentifier
            element LPar
            parameters <- parseP
            element RPar
            rtw <- parseR
            element EqualsSign
            return $ Def (IW functionName) (reverse parameters) rtw

parseN :: Parser Token (IdentifierWrapper, Maybe TypeHintWrapper)
parseN = do Identifier parameterName <- isIdentifier
            thw <- parseT
            return $ (IW parameterName, thw)

parseT :: Parser Token (Maybe TypeHintWrapper)
parseT = do element Colon
            thw <- parseT'
            return $ Just thw
     <|> return Nothing

parseT' :: Parser Token TypeHintWrapper
parseT' = do TypeHint typeHint <- isTypeHint
             return $ THW typeHint

parseP :: Parser Token [(IdentifierWrapper, Maybe TypeHintWrapper)]
parseP = (parseN >>= \x -> parseP' [x])
     <|> return []

parseP' :: [(IdentifierWrapper, Maybe TypeHintWrapper)] 
        -> Parser Token [(IdentifierWrapper, Maybe TypeHintWrapper)]
parseP' ids = do element Comma
                 pair <- parseN
                 parseP' (pair : ids)
          <|> return ids

parseR :: Parser Token (Maybe TypeHintWrapper)
parseR = do element Arrow
            thw <- parseT'
            return $ Just thw
     <|> return Nothing

----------------------

parse' :: String -> Maybe Def
parse' = (\case [] -> Nothing; (x : xs) -> Just x) . evalParser (parseS <* eof) . tokenize

parse :: String -> Def
parse = head . evalParser (parseS <* eof) . tokenize