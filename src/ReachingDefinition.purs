module ReachingDefinition where

import Data.List (List(..), (:), length, singleton, null, sortBy, uncons, unsnoc, nubBy)
import AST
import ProgramGraph
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Generator
import Data.Either (Either(..))
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition)
import Prelude (show, bind, pure, show, ($), (+), (-), (<>), (<), (==), negate)

rdGenerate :: Either ParseError Program -> String
rdGenerate (Left err) =
  let message = parseErrorMessage err in
  let pos = showPosition $ parseErrorPosition err in
  "Error: " <> message <> " at " <> pos
rdGenerate (Right p) = let edges = pgProgram p in case p of 
  Program d s -> initReachingDefinition edges (defineVariables d Nil) <> (initPG edges)


initReachingDefinition :: List Edge -> List Element -> List (List (Tuple Element (Tuple Int Int)))
initReachingDefinition edges elements = 
  let cs = constraints edges in
  let firstRD = unknownDefinition elements 0 in
  let nextConstraints = findConstraint cs 0 in


reachingDefinition :: List (Tuple Element (Tuple Int Int)) -> List (Tuple (Tuple Int Int) (Tuple Set Set)) -> List (List (Tuple Element (Tuple Int Int)))
reachingDefinition previousRD constraints = rd 



findConstraint :: List (Tuple (Tuple Int Int) (Tuple Set Set)) -> Int -> List (Tuple (Tuple Int Int) (Tuple Set Set))
findConstraint ((x /\ z) /\ (a /\ b):xs) y = 
  if y == x 
  then ((x /\ z) /\ (a /\ b):findConstraint xs y)
  else findConstraint xs y
findConstraint Nil _ = Nil


constraints :: List Edge -> List (Tuple (Tuple Int Int) (Tuple Set Set))
constraints (e:es) = (constraint e:constraints es)
constraints Nil = Nil

constraint :: Edge -> Tuple (Tuple Int Int) (Tuple Set Set)
constraint (E x y z) = case y of
  D a -> case a of 
      DVar b -> (x /\ z) /\ (Neither /\ Gen (Var b) x z)
      DArray b c -> (x /\ z) /\ (Neither /\ Gen (Array b) x z)
      DRecord b -> (x /\ z) /\ (Neither /\ Gen (Record b) x z) 
      _ -> (x /\ z) /\ (Neither /\ Neither)
  S a -> case a of 
      LDef b c -> case b of 
        LVar o -> (x /\ z) /\ (Kill (Var o) /\ Gen (Var o) x z)
        LArray o _ -> (x /\ z) /\ (Kill (Array o) /\ Gen (Array o) x z)
        LRfst o -> (x /\ z) /\ (Kill (Record o) /\ Gen (Record o) x z)
        LRsnd o -> (x /\ z) /\ (Kill (Record o) /\ Gen (Record o) x z) 
      RDef b c d -> (x /\ z) /\ (Kill (Record b) /\ Gen (Record b) x z) 
      Read b -> case b of 
        LVar o -> (x /\ z) /\ (Neither /\ Gen (Var o) x z)
        LArray o _ -> (x /\ z) /\ (Neither /\ Gen (Array o) x z)
        LRfst o -> (x /\ z) /\ (Neither /\ Gen (Record o) x z)
        LRsnd o -> (x /\ z) /\ (Neither /\ Gen (Record o) x z) 
      Write b -> (x /\ z) /\ (Neither /\ Neither)
      _ -> (x /\ z) /\ (Neither /\ Neither)
  B a -> (x /\ z) /\ (Neither /\ Neither)
   
unknownDefinition :: List Element -> Int -> List (Tuple Element (Tuple Int Int))
unknownDefinition (e:es) d = ((e /\ ((-1) /\ d)):unknownDefinition es d)
unknownDefinition Nil d = Nil


data ReachingDefinition = RD Int (List Assignment)
                          -- Node|Reaching Definition
data Assignment         = A Element Int Int
                         -- Var/Arr|In |Out
data Constraint         = C Int Int Set Set
                         -- In |Out|Kill|Gen
data Element 
  = Var String 
  | Array String 
  | Record String 

data Set 
  = Kill Element
  | Gen Element Int Int
  | Neither
  
data Genset 
  = Gen Element Int Int 
  | NoGen

data Killset 
  = Kill Element
  | NoKill

findEdge :: List Edge -> Int -> List Edge
findEdge (E a b c:le) d = 
    if c == d 
    then (E a b c:findEdge le d) 
    else (findEdge le d)
findEdge Nil d = Nil 

findExitingEdge :: List Edge -> Int -> List Edge
findExitingEdge (E a b c:le) d = 
    if a == d 
    then (E a b c:findExitingEdge le d) 
    else (findExitingEdge le d)
findExitingEdge Nil d = Nil 

defineVariables :: Declaration -> List Element -> List Element
defineVariables d le = case d of 
  DVar s -> replaceElement (Var s) le 
  DArray s i -> replaceElement (Array s) le 
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
name (Array a) = a
name (Record a) = a


{-
printElement :: List Element -> String
printElement (Var a:as) = "Var " <> a <> """
""" <> printElement as
printElement (Array a:as) = "Array " <> a <> """
""" <> printElement as
printElement (Record a:as) = "Record " <> a <> """
""" <> printElement as
printElement Nil = ""

printDeclarations :: List Element -> String 
printDeclarations le = """/*
""" <> printElement le <> """*/
"""-}

printReachingDefinitions :: List (List (Tuple Element (Tuple Int Int))) -> Int -> String
printReachingDefinitions (a:as) e = "RD(q" <> show e <> ")={" <> printRD a <> """}
""" <> printReachingDefinitions as (e+1)
printReachingDefinitions Nil _ = ""

printRD :: List (Tuple Element (Tuple Int Int)) -> String 
printRD (a:as) = "(" <> strRD a <> ")" <> printRD as
printRD Nil = ""

strRD :: Tuple Element (Tuple Int Int) -> String 
strRD (e /\ (a /\ b)) = let r = "," <> (if a == -1 then "?" else show a) <> "," <> show b in
  case e of 
    Var f -> f <> r
    Array f -> f <> r
    Record f -> f <> r

