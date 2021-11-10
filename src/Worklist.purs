module Worklist where 

import Data.List (List(..), (:), snoc, nubBy, null, concat)
import Data.Maybe
import Data.Tuple 
import Prelude ((==),not , (<>), show, ($), (&&))
import Data.Tuple.Nested ((/\))
import ProgramGraph
import AST
import Data.Set (Set(..), empty, singleton, member, insert, findMin, delete, union)

type WorklistStack = List Int

emptyStack :: WorklistStack
emptyStack = Nil

extractStack :: WorklistStack -> Maybe (Tuple Int WorklistStack)
extractStack (w:k) = Just (w /\ k)
extractStack Nil = Nothing

insertStack :: WorklistStack -> Int -> WorklistStack
insertStack w i = (i:w)


type WorklistRobin = Tuple (List Int) Boolean

emptyRobin :: WorklistRobin
emptyRobin = Nil /\ false

insertRobin :: Int -> WorklistRobin -> WorklistRobin
insertRobin q (v /\ t) = ((q:v) /\ true)
 
extractRobin :: WorklistRobin -> Maybe (Tuple Int WorklistRobin)
extractRobin (v /\ t) = case v of 
    (head:tail) -> Just (head /\ (tail /\ t))
    Nil -> Just $ 0 /\ (v /\ false)
extractRobin (Nil /\ t) = Nothing





worklistRun :: Program -> String
worklistRun p = printdfs $ depthFirstSpanning $ pgProgram p



depthFirstSpanning :: List Edge -> Tuple (Set (Tuple Int Int)) (List Int) 
depthFirstSpanning edges =
    let t = empty in
    let v = singleton 0 in
    let k = 0 in 
    let rP = (0:Nil) in
    case dfsRec edges t v k rP of 
        (a /\ b) -> (a /\ postProcess b empty)


postProcess :: List Int -> Set Int -> List Int
postProcess (a:as) b = 
    if member a b
    then postProcess as b
    else (a:postProcess as (insert a b))
postProcess Nil _ = Nil
    

dfsRec :: List Edge -> Set (Tuple Int Int) -> Set Int -> Int -> List Int -> Tuple (Set (Tuple Int Int)) (List Int)
dfsRec edges t v k rP = case findEdges edges v k of 
    Nil -> t /\ (nubBy (==) rP)
    newEdges -> dfsRec2 edges t v k rP newEdges


dfsRec2 :: List Edge -> Set (Tuple Int Int) -> Set Int -> Int -> List Int -> List Edge -> Tuple (Set (Tuple Int Int)) (List Int)
dfsRec2 edges t v k rP (E a b c:as) = 
    let vNew = insert k v in 
    let tNew = insert (a /\ c) t in 
    let rPNew = snoc rP c in 
    mergeTuples (dfsRec edges tNew vNew c rPNew) (dfsRec2 edges t v k rP as)
dfsRec2 _ _ _ _ _ Nil = (empty /\ Nil)

mergeTuples :: Tuple (Set (Tuple Int Int)) (List Int) -> Tuple (Set (Tuple Int Int)) (List Int) -> Tuple (Set (Tuple Int Int)) (List Int)
mergeTuples (a /\ b) (c /\ d) = (union a c) /\ (concat (b:(d:Nil)))




findEdges :: List Edge -> Set Int -> Int -> List Edge
findEdges (E a b c:e) d f =
  if not (member c d) && a == f 
  then (E a b c:findEdges e d f)
  else findEdges e d f
findEdges Nil d _ = Nil 




printdfs :: Tuple (Set (Tuple Int Int)) (List Int) -> String
printdfs (a /\ b) = printST a <> return <> printL b

printST :: Set (Tuple Int Int) -> String
printST a =
    let b = findMin a in
    case b of 
        Nothing -> ""
        Just (d /\ e) -> let c = delete (d /\ e) a in 
            show d <> " " <> show e <> return <> printST c

return :: String
return = """
"""

printL :: List Int -> String
printL (a:as) = show a <> " " <> printL as
printL Nil = ""










