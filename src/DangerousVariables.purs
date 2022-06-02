module DangerousVariables where

import AST (LExp(..), Program(..), Statement(..))
import ReachingDefinition (defineVariables, defineVariablesStatement, fvAExp, mergeElement, replaceElement) 
import ProgramGraph (highest, pgProgram) 
import Basic (Element(..), eqElement, name, Edge(..), Content(..))
import AllTraversals (initAllTraversals)
import Data.List (List(..), concat, intersectBy, nubByEq, null, reverse, (:))
import Prelude (show, ($), (-), (<>), (==))


dvGenerate :: Program -> String
dvGenerate p = let edges = pgProgram p in case p of 
  Program d s -> let els = nubByEq eqElement $ mergeElement (defineVariables d Nil) (defineVariablesStatement s) in 
    printDangerousVariables (initDV edges els)


initDV :: List Edge -> List Element -> List DangerousVariable
initDV edges elements = 
  let firstDV = (DV 0 elements) in
  let limit = highest edges 0 in
  reverse $ assemble (firstDV:concat (recDV (initAllTraversals edges) firstDV)) limit

assemble :: List DangerousVariable -> Int -> List DangerousVariable
assemble as (-1) = Nil 
assemble as i =
  case nubByEq eqElement (findDV as i) of
    Nil -> (DV i Nil:assemble as (i-1))
    r -> (DV i r:assemble as (i-1))

findDV :: List DangerousVariable -> Int -> List Element 
findDV (DV i as:s) u = if i == u then mergeElement as (findDV s u) else findDV s u
findDV Nil _ = Nil 



recDV :: List (List Edge) -> DangerousVariable -> List (List DangerousVariable)
recDV (a:as) r = (dangerousVariable a r:recDV as r)
recDV Nil r = Nil 

dangerousVariable :: List Edge -> DangerousVariable -> List DangerousVariable 
dangerousVariable (a:as) b = 
  let newDV = solveConstraint b a in
  (newDV:dangerousVariable as newDV)
dangerousVariable Nil _ = Nil

solveConstraint :: DangerousVariable -> Edge -> DangerousVariable
solveConstraint (DV i is) (E _ element node) = 
  case element of 
  D a -> DV node is
  S a -> case a of 
      LDef b c -> case b of 
        LVar o -> if null $ intersectBy eqElement is (fvAExp c) then DV node (removeElement is (Var o)) else DV node (replaceElement (Var o) is)
        LArray o x -> if null $ intersectBy eqElement is (mergeElement (fvAExp c) (fvAExp x)) then DV node is else DV node (replaceElement (Array o) is)
        LRfst o ->  if null $ intersectBy eqElement is (fvAExp c) then DV node (removeElement is (Record o)) else DV node (replaceElement (Record o) is)
        LRsnd o ->  if null $ intersectBy eqElement is (fvAExp c) then DV node (removeElement is (Record o)) else DV node (replaceElement (Record o) is)
      RDef b c d -> if null $ intersectBy eqElement is (fvAExp c) then DV node (removeElement is (Record b)) else DV node (replaceElement (Record b) is)
      Read b -> case b of 
        LVar o -> DV node $ removeElement is (Var o)
        LArray o x -> if null $ intersectBy eqElement is ((fvAExp x)) then DV node is else DV node (replaceElement (Array o) is)
        LRfst o -> DV node $ removeElement is (Record o) 
        LRsnd o -> DV node $ removeElement is (Record o)  
      Write b -> DV node is 
      _ -> DV node Nil 
  B a -> DV node is 

removeElement :: List Element -> Element -> List Element
removeElement (a:as) d = 
  if name a == name d
  then removeElement as d
  else (a:removeElement as d)
removeElement Nil _ = Nil 

data DangerousVariable = DV Int (List Element)

printDangerousVariables :: List (DangerousVariable) -> String
printDangerousVariables (DV a b:as) = "DV(q" <> show a <> ")={" <> printDV b <> """}
""" <> printDangerousVariables as 
printDangerousVariables Nil = ""

printDV :: List (Element) -> String 
printDV (a:Nil) = strDV a
printDV (a:as) = strDV a <> "," <> printDV as
printDV Nil = ""

strDV :: Element -> String 
strDV e =
  case e of 
    Var f -> f 
    Array f -> f
    Record f -> f
