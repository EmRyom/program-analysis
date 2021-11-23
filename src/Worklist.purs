module Worklist where 

import Data.CatList
import Data.Maybe
import Data.Tuple 
import Data.Eq
import Basic 
import Data.Ordering
import Prelude ((==), not, (<>), show, ($), (&&), (>))
import Data.Tuple.Nested ((/\))
import ProgramGraph
import AST
import Data.Set (Set(..), empty, singleton, member, insert, findMin, delete, union)

type Worklist = CatList Int 

empty :: Worklist
empty = CatNil

extract :: Worklist -> Maybe (Tuple Int Worklist)
extract e = uncons e

insertStack :: Worklist -> Int -> Worklist
insertStack w i = cons i w

insertQueue :: Worklist -> Int -> Worklist
insertQueue w i = snoc w i 

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















