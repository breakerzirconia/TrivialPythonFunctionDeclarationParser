{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}

module LL1.Combinators where

import Data.Functor ((<&>))
import Data.Maybe   (fromJust)
import Control.Applicative
import Def
import LL1.Parser
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
parseS = (element DefKeyword *> pure Def)
     <*> (isIdentifier <&> \ ~(Identifier x) -> IW x)
     <*> (element LPar *> parseP <* element RPar) 
     <*> (parseR <* element EqualsSign)

parseN :: Parser Token (IdentifierWrapper, Maybe TypeHintWrapper)
parseN = liftA2 (,) (isIdentifier <&> \ ~(Identifier x) -> IW x) parseT

parseT :: Parser Token (Maybe TypeHintWrapper)
parseT = (pure Just <*> (element Colon *> parseT')) <|> pure Nothing

parseT' :: Parser Token TypeHintWrapper
parseT' = isTypeHint <&> \ ~(TypeHint x) -> THW x

parseP :: Parser Token [(IdentifierWrapper, Maybe TypeHintWrapper)]
parseP = (liftA2 (:) parseN $ many (element Comma *> parseN)) <|> pure []

parseR :: Parser Token (Maybe TypeHintWrapper)
parseR = (pure Just <*> (element Arrow *> parseT')) <|> pure Nothing

----------------------

parse' :: String -> Maybe Def
parse' = evalParser parseS . tokenize

parse :: String -> Def
parse = fromJust . evalParser parseS . tokenize