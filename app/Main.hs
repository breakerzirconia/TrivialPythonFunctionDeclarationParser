module Main where

import Control.Monad.State
import Def
import LL1.Combinators
import System.Process

quoteToDoubleApostrophe :: Show v => v -> String
quoteToDoubleApostrophe = concatMap (\c -> case c of '"' -> "''"; c -> [c]) . show

class Graph v where
  form :: v -> State Int String

form0 :: Show v => v -> State Int String
form0 v = state $ \n -> ( "  n" ++ show n 
                                ++ " [label=\"" 
                                ++ quoteToDoubleApostrophe v 
                                ++ "\"] ;\n"
                        , succ n
                        )

form1 :: Graph v1 => String -> v1 -> State Int String
form1 str v1 = state $ \n -> let (str1, n1) = runState (form v1) $ n + 1
                             in ("  n" ++ show n 
                                       ++ " [label=\"" 
                                       ++ str 
                                       ++ "\"] ;\n" 
                                       ++ "  n" 
                                       ++ show n 
                                       ++ " -- n" 
                                       ++ show (n + 1) 
                                       ++ " ;\n" 
                                       ++ str1, n1)

form2 :: (Graph v1, Graph v2) => String -> v1 -> v2 -> State Int String
form2 str v1 v2 = state $ \n -> let (str1, n1) = runState (form v1) $ n + 1
                                    (str2, n2) = runState (form v2) n1
                                in ("  n" ++ show n 
                                          ++ " [label=\"" 
                                          ++ str 
                                          ++ "\"] ;\n" 
                                          ++ "  n" 
                                          ++ show n 
                                          ++ " -- n" 
                                          ++ show (n + 1) 
                                          ++ " ;\n" 
                                          ++ str1 
                                          ++ "  n" 
                                          ++ show n 
                                          ++ " -- n" 
                                          ++ show n1 
                                          ++ " ;\n" 
                                          ++ str2, n2)

form3 :: (Graph v1, Graph v2, Graph v3) => String -> v1 -> v2 -> v3 -> State Int String
form3 str v1 v2 v3 = state $ \n -> let (str1, n1) = runState (form v1) $ n + 1
                                       (str2, n2) = runState (form v2) n1
                                       (str3, n3) = runState (form v3) n2
                                   in ("  n" ++ show n 
                                             ++ " [label=\"" 
                                             ++ str 
                                             ++ "\"] ;\n" 
                                             ++ "  n" 
                                             ++ show n 
                                             ++ " -- n" 
                                             ++ show (n + 1) 
                                             ++ " ;\n" 
                                             ++ str1 
                                             ++ "  n" 
                                             ++ show n 
                                             ++ " -- n" 
                                             ++ show n1 
                                             ++ " ;\n" 
                                             ++ str2
                                             ++ "  n" 
                                             ++ show n 
                                             ++ " -- n" 
                                             ++ show n2 
                                             ++ " ;\n" 
                                             ++ str3, n3)

instance Graph Def where
  form (Def name parameters returnType) = form3 "Def" name parameters returnType

instance Graph v => Graph [v] where
  form []       = form0 ([] :: [Int])
  form (x : xs) = form2 ":" x xs

instance Graph v => Graph (Maybe v) where
  form Nothing = form0 (Nothing :: Maybe Int)
  form (Just x) = form1 "Just" x

instance (Graph v1, Graph v2) => Graph (v1, v2) where
  form (v1, v2) = form2 "(,)" v1 v2

instance Graph IdentifierWrapper where
  form (IW s) = form0 s

instance Graph TypeHintWrapper where
  form (THW s) = form0 s 

graph :: String -> String
graph str = case parse' str of
  Just def -> "graph \"\"\n  {\n  label=\"" ++ quoteToDoubleApostrophe str 
                                            ++ "\"\n  n1 ;\n" 
                                            ++ fst (runState (form def) 1) 
                                            ++ "  }\n"
  Nothing  -> "graph \"\"\n  {\n  label=\"Nothing\"\n  }\n"


main :: IO ()
main = getLine >>= \str -> writeFile "Graph.gv" (graph str) >>
                           callCommand "dot -Tjpg Graph.gv -o Graph.jpg"