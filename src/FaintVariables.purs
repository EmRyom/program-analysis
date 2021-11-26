module FaintVariables where

import AST
import ProgramGraph (pgProgram, highest)
import ReachingDefinition (fvAExp, fvBExp, eqElement, Element(..))
import AllTraversals (initAllTraversals)
import LiveVariables (reverseEdges, Genset(..), Killset(..), mergeElement)
import Data.List (List(..), concat, nubBy, singleton, (:), deleteBy, unionBy, reverse, null, intersectBy)
import Prelude (negate, show, ($), (&&), (-), (<>), (==))
import Basic
import DangerousVariables (removeElement)

fvGenerate :: Program -> String
fvGenerate p = let edges = pgProgram p in case p of 
  Program d s -> printFaintVariables $ reverse (initFV edges)


initFV :: List Edge -> List StronglyLiveVariable
initFV edges =
  let limit = highest edges 0 in 
  assemble (concat (recFV (initAllTraversals edges) (SLV limit Nil))) limit

assemble :: List StronglyLiveVariable -> Int -> List StronglyLiveVariable
assemble as -1 = Nil 
assemble as i =
  case nubBy eqElement (findFV as i) of
    Nil -> (SLV i Nil:assemble as (i-1))
    r -> (SLV i r:assemble as (i-1))


findFV :: List StronglyLiveVariable -> Int -> List Element 
findFV (SLV i as:s) u = if i == u then mergeElement as (findFV s u) else findFV s u
findFV Nil _ = Nil 

recFV :: List (List Edge) -> StronglyLiveVariable -> List (List StronglyLiveVariable)
recFV (a:as) r = ((faintVariable (reverse $ reverseEdges a) r):recFV as r)
recFV Nil r = Nil

faintVariable :: List Edge -> StronglyLiveVariable -> List StronglyLiveVariable 
faintVariable (a:as) b = 
  let newLV = solveConstraint b a in
  (newLV:faintVariable as newLV)
faintVariable Nil _ = Nil

contains :: Element -> List Element -> Boolean
contains a (b:bs) = if eqElement a b then true else contains a bs
contains a Nil = false 

solveConstraint :: StronglyLiveVariable -> Edge -> StronglyLiveVariable
solveConstraint (SLV i a) (E _ element node) = SLV node (case element of 
    D _ -> a
    S s -> case s of 
      LDef l1 l2 -> case l1 of 
        LVar l3 -> if contains (Var l3) a then mergeElement (removeElement a (Var l3)) (fvAExp l2) else a
        LArray l3 l4 -> if contains (Array l3) a then mergeElement (mergeElement a $ fvAExp l4) (fvAExp l2) else a
        LRfst l3 -> if contains (Record l3) a then mergeElement (removeElement a (Record l3)) (fvAExp l2) else a 
        LRsnd l3 -> if contains (Record l3) a then mergeElement (removeElement a (Record l3)) (fvAExp l2) else a
      RDef l1 l2 l3 -> if contains (Record l1) a then mergeElement (mergeElement (removeElement a (Record l1)) (fvAExp l3)) (fvAExp l2) else a
      Read l1 -> case l1 of
        LVar l3 -> removeElement a (Var l3)
        LArray l3 l4 -> if contains (Array l3) a then mergeElement a (fvAExp l4) else a
        LRfst l3 -> removeElement a (Record l3)
        LRsnd l3 -> removeElement a (Record l3)
      Write l1 -> mergeElement a (fvAExp l1)
      _ -> a
    B b -> mergeElement a (fvBExp b))

data StronglyLiveVariable = SLV Int (List Element)

printFaintVariables :: List (StronglyLiveVariable) -> String
printFaintVariables (SLV a b:as) = "SLV(q" <> show a <> ")={" <> printFV b <> """}
""" <> printFaintVariables as 
printFaintVariables Nil = ""

printFV :: List (Element) -> String 
printFV (a:Nil) = case a of 
  Var b -> b
  Record b -> b
  Array b -> b
printFV (Var a:as) = a <> "," <> printFV as
printFV (Record a:as) = a <> "," <> printFV as
printFV (Array a:as) = a <> "," <> printFV as
printFV Nil = ""