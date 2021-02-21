module Def where

type Name      = String
type Parameter = String

data IdentifierWrapper = IW String deriving (Eq, Ord, Show)
data TypeHintWrapper = THW String deriving (Eq, Ord, Show)

data Def = Def { name       :: IdentifierWrapper                            -- ^ Function name
               , parameters :: [(IdentifierWrapper, Maybe TypeHintWrapper)] -- ^ List of the parameter names, 
                                                                            --   may contain a type hint
               , returnType :: Maybe TypeHintWrapper                        -- ^ Function return type
               } deriving (Eq, Ord, Show)