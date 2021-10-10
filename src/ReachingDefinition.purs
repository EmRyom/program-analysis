module ReachingDefinition where

import Data.List (List(..), (:), head, length, singleton, null, sortBy, uncons, unsnoc, nubBy, reverse)
import AST
import Basic
import ProgramGraph
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Generator
import Data.Either (Either(..))
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition)
import Data.Maybe
import Prelude (show, bind, pure, show, (&&), ($), (+), (-), (<>), (<), (==), negate)

rdGenerate :: Either ParseError Program -> String
rdGenerate (Left err) =
  let message = parseErrorMessage err in
  let pos = showPosition $ parseErrorPosition err in
  "Error: " <> message <> " at " <> pos
rdGenerate (Right p) = let edges = pgProgram p in case p of 
  Program d s -> """/*
""" <> printReachingDefinitions (initRD edges (defineVariables d Nil)) <> """*/
""" <> initPG edges 


initRD :: List Edge -> List Element -> List ReachingDefinition
initRD edges elements = 
  let firstRD = unknownDefinition elements 0 in
  let initRun = firstRun edges firstRD in 
  case initRun of 
  (a:as) -> mergeRD (reverse initRun) (run edges a Nil) 
  Nil -> Nil


mergeRD :: List ReachingDefinition -> List ReachingDefinition -> List ReachingDefinition
mergeRD (a:as) b = mergeRD as (a:b)
mergeRD Nil b = b

firstRun :: List Edge -> ReachingDefinition -> List ReachingDefinition
firstRun cs (RD i as) = case findEdge cs i of 
  Nothing -> Nil
  Just c -> let newRD = solveConstraint (RD i as) c in
    (newRD:firstRun cs newRD)




run :: List Edge -> ReachingDefinition -> List Edge -> List ReachingDefinition
run cs (RD i as) disc = case findConsDisc cs i disc of 
  Nothing -> Nil
  Just c -> let newRD = solveConstraint (RD i as) c in 
    (newRD:run cs newRD (c:disc))


findConsDisc :: List Edge -> Int -> List Edge -> Maybe Edge 
findConsDisc edges i disc = 
  let candidates = findEdges edges i in 
  case difference candidates disc of 
  Nil -> head candidates
  (a:as) -> Just a 


difference :: List Edge -> List Edge -> List Edge
difference (a:as) b = if contains a b then difference as b else (a:difference as b)
difference Nil _ = Nil

{-contains :: Edge -> List Edge -> Boolean
contains c (d:ds) = if eqEdge c d then true else contains c ds
contains c Nil = false -}

eqConstraint :: Constraint -> Constraint -> Boolean
eqConstraint (C a b c d) (C e f g h) = a == e && b == f && eqKill c g && eqGen d h

eqKill :: Killset -> Killset -> Boolean
eqKill (Kill a) (Kill b) = eqElement a b
eqKill NoKill NoKill = true 
eqKill _ _ = false

eqGen :: Genset -> Genset -> Boolean
eqGen (Gen a b c) (Gen d e f) = eqElement a d && b == e && c == f
eqGen NoGen NoGen = true
eqGen _ _ = false 

eqElement :: Element -> Element -> Boolean
eqElement (Var a) (Var b) = a == b
eqElement (Array a) (Array b) = a == b
eqElement (Record a) (Record b) = a == b
eqElement _ _ = false 

findEdges :: List Edge -> Int -> List Edge
findEdges (E out c inn:edges) i = 
  if out == i then (E out c inn:findEdges edges i)
  else findEdges edges i
findEdges Nil _ = Nil 

findEdge :: List Edge -> Int -> Maybe Edge
findEdge (E out c inn:edges) i = 
  if out == i then Just (E out c inn)
  else findEdge edges i
findEdge Nil _ = Nothing  

makeEmptyRD :: Int -> List ReachingDefinition
makeEmptyRD 0 = Nil
makeEmptyRD a = (RD a Nil: makeEmptyRD (a-1)) 
  
solveConstraint :: ReachingDefinition -> Edge -> ReachingDefinition
solveConstraint (RD i a) edge = 
  case constraint edge of 
    (C _ o k g) -> case k of 
      Kill e -> let newAl = removeElement a e in 
        case g of 
          Gen x y z -> RD o (A x y z:newAl)
          NoGen -> RD o newAl
      NoKill -> case g of 
        Gen x y z -> RD o (A x y z:a)
        NoGen -> RD o a 

removeElement :: List Assignment -> Element -> List Assignment
removeElement (A a b c:as) d = 
  if name a == name d
  then removeElement as d
  else (A a b c:removeElement as d)
removeElement Nil _ = Nil 

constraint :: Edge -> Constraint
constraint (E x y z) = case y of
  D a -> case a of 
      DVar b -> C x z NoKill (Gen (Var b) x z)
      DArray b c -> C x z NoKill $ Gen (Array b) x z
      DRecord b -> C x z NoKill $ Gen (Record b) x z 
      _ -> C x z NoKill NoGen
  S a -> case a of 
      LDef b c -> case b of 
        LVar o -> C x z (Kill (Var o)) $ Gen (Var o) x z
        LArray o _ -> C x z (Kill (Array o)) $ Gen (Array o) x z
        LRfst o ->  C x z (Kill (Record o)) $ Gen (Record o) x z
        LRsnd o ->  C x z (Kill (Record o)) $ Gen (Record o) x z
      RDef b c d -> C x z (Kill (Record b)) $ Gen (Record b) x z
      Read b -> case b of 
        LVar o ->  C x z (Kill (Var o)) $ Gen (Var o) x z
        LArray o _ ->  C x z NoKill $ Gen (Array o) x z
        LRfst o -> C x z (Kill (Record o)) $ Gen (Record o) x z
        LRsnd o -> C x z (Kill (Record o)) $ Gen (Record o) x z
      Write b -> C x z NoKill NoGen
      _ -> C x z NoKill NoGen
  B a -> C x z NoKill NoGen
   
unknownDefinition :: List Element -> Int -> ReachingDefinition
unknownDefinition e d = (RD d (ud e d))

ud :: List Element -> Int -> List Assignment
ud (e:es) d = (A e (-1) d:ud es d)
ud Nil _ = Nil


data ReachingDefinition = RD Int (List Assignment)
                          -- Node|Reaching Definition
data Assignment         = A Element Int Int
                         -- Var/Arr|Out|In
data Constraint         = C Int Int Killset Genset
                         -- Out|In|Kill|Gen
data Element 
  = Var String 
  | Array String 
  | Record String 
data Genset 
  = Gen Element Int Int 
  | NoGen
data Killset 
  = Kill Element
  | NoKill


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

printReachingDefinitions :: List (ReachingDefinition) -> String
printReachingDefinitions (RD a b:as) = "RD(q" <> show a <> ")={" <> printRD b <> """}
""" <> printReachingDefinitions as 
printReachingDefinitions Nil = ""

printRD :: List (Assignment) -> String 
printRD (a:as) = "(" <> strRD a <> ")" <> printRD as
printRD Nil = ""

strRD :: Assignment -> String 
strRD (A e a b) = let r = "," <> (if a == -1 then "?" else show a) <> "," <> show b in
  case e of 
    Var f -> f <> r
    Array f -> f <> r
    Record f -> f <> r
