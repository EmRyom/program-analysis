module Worklist where 

import Data.CatList
import Data.Maybe
import Data.Tuple 
import Data.Tuple.Nested ((/\))
import Data.Eq
import Basic 
import Data.Ordering
import Prelude ((==), not, (<>), show, ($), (&&), (>), (+))
import ProgramGraph
import AST
import Data.List (List(..), (:), nubBy, concat)
import Data.Set (Set(..), empty, singleton, member, findMin, delete, union)
import ReachingDefinition (Assignment(..), ReachingDefinition(..), mergeElement, eqAssignment, printReachingDefinitions, mergeAssignment, solveConstraint, unknownDefinition, defineVariables, defineVariablesStatement)

type Worklist = CatList Int 

empty :: Worklist
empty = CatNil

extract :: Worklist -> Maybe (Tuple Int Worklist)
extract e = uncons e

--                           Stack? (Otherwise queue)
insert :: Worklist -> Int -> Boolean -> Worklist
insert w i b = if b then cons i w else snoc w i


initAsQueue :: List Edge -> List Int
initAsQueue edges = let high = highest edges 0 in rangeQueue 0 high

rangeQueue :: Int -> Int -> List Int
rangeQueue a b = if a == b then (a:Nil) else (a:rangeQueue (a+1) b)

worklistQueue :: List Int -> CatList Int -> CatList Int
worklistQueue (a:as) es = worklistQueue as (insert es a false)
worklistQueue Nil es = es


worklistStack :: List Int -> CatList Int -> CatList Int
worklistStack (a:as) es = worklistStack as (insert es a true)
worklistStack Nil es = es







{-
type WorklistStack = CatList Int

emptyStack :: WorklistStack
emptyStack = CatNil

extractStack :: WorklistStack -> Maybe (Tuple Int WorklistStack)
extractStack e = uncons e

insertStack :: WorklistStack -> Int -> WorklistStack
insertStack w i = cons i w

type WorklistQueue = CatList Int

emptyQueue :: WorklistQueue
emptyQueue = CatNil

extractQueue :: WorklistQueue -> Maybe (Tuple Int WorklistQueue)
extractQueue e = uncons e

insertQueue :: WorklistQueue -> Int -> WorklistQueue
insertQueue w i = snoc w i -}















