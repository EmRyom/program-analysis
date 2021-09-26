module ReachingDefinition where

import Data.List (List(..), (:), length, singleton, null, sortBy, uncons, unsnoc, nubBy)
import AST
import ProgramGraph
import Generator
import Data.Either (Either(..))
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition)
import Prelude (show, bind, pure, show, ($), (+), (-), (<>), (<), (==))
import Text.Parsing.Parser.Pos (Position(..))


rdGenerate :: Either ParseError Program -> String
rdGenerate (Left err) =
  let message = parseErrorMessage err in
  let pos = showPosition $ parseErrorPosition err in
  "Error: " <> message <> " at " <> pos
rdGenerate (Right p) = case p of 
  Program d s -> printElement (defineVariables d Nil) <> (initPG $ pgProgram p)

{-}
reachingDefinition :: List Element -> List Edge -> List List (Tuple Element (Tuple Int Int))
reachingDefinition (e:es) le = 


reachingDefSingle :: Element -> List Edge -> List (Tuple Element (Tuple Int Int))
reachingDefSingle e le = let edges = findEdge le 0 in
-}


findEdge :: List Edge -> Int -> List Edge
findEdge (E a b c:le) d = 
    if a == d 
    then (E a b c:findEdge le d) 
    else (findEdge le d)
findEdge Nil d = Nil 



defineVariables :: Declaration -> List Element -> List Element
defineVariables d le = case d of 
  DVar s -> replaceElement (Var s) le 
  DArray s i -> replaceElement (Array s i) le 
  DRecord s -> replaceElement (Record s) le  
  None -> le 
  DDouble d1 d2 -> defineVariables d2 $ defineVariables d1 le 

replaceElement :: Element -> List Element -> List Element
replaceElement e (a:as) =
  if name a == name e 
  then replaceElement e as
  else (a:replaceElement e as)
replaceElement e Nil = singleton e 

name :: Element -> String
name (Var a) = a
name (Array a b) = a
name (Record a) = a

data Element 
  = Var String 
  | Array String Int 
  | Record String 

printElement :: List Element -> String
printElement (Var a:as) = "Var " <> a <> """
""" <> printElement as
printElement (Array a b:as) = "Array " <> a <> " Length " <> show b <> """
""" <> printElement as
printElement (Record a:as) = "Record " <> a <> """
""" <> printElement as
printElement Nil = ""

