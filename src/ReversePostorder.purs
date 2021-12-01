module ReversePostorder where


import Basic
import AST
import Data.Set (Set(..), empty, singleton, member, findMin, delete, union, insert)
import Prelude
import Data.List (List(..), (:), nubBy, concat, sortBy, snoc)
import Data.Tuple 
import Data.Tuple.Nested ((/\))

type ReversePostorder = List Int
type T = Set Edge

depthFirstSpanning :: List Edge -> Tuple T ReversePostorder
depthFirstSpanning edges =
    let t = empty in
    let v = empty in
    let k = 0 in 
    let rP = (Nil) in
    case dfsRec edges t v k rP of 
        (a /\ b) -> (a /\ (0:postProcess b empty))


postProcess :: List Int -> Set Int -> List Int
postProcess (a:as) b = 
    if member a b
    then postProcess as b
    else (a:postProcess as (insert a b))
postProcess Nil _ = Nil
    

dfsRec :: List Edge -> Set Edge -> Set Int -> Int -> List Int -> Tuple (T) (ReversePostorder)
dfsRec edges t v k rP = case findEdges edges v k of 
    Nil -> t /\ (nubBy (==) rP)
    newEdges -> dfsRec2 edges t v k rP $ sortBy lowest newEdges -- MAYBE NO SORTBY

lowest :: Edge -> Edge -> Ordering
lowest (E a b c) (E d e f) = if c > f then GT else LT



dfsRec2 :: List Edge -> Set Edge -> Set Int -> Int -> List Int -> List Edge -> Tuple T ReversePostorder
dfsRec2 edges t v k rP (E a b c:as) = 
    let vNew = insert k v in 
    let tNew = insert (E a b c) t in 
    let rPNew = snoc rP c in 
    mergeTuples (dfsRec edges tNew vNew c rPNew) (dfsRec2 edges t v k rP as)
dfsRec2 _ _ _ _ _ Nil = (empty /\ Nil)

mergeTuples :: Tuple T ReversePostorder -> Tuple T ReversePostorder -> Tuple T ReversePostorder
mergeTuples (a /\ b) (c /\ d) = (union a c) /\ (concat (b:(d:Nil)))



findEdges :: List Edge -> Set Int -> Int -> List Edge
findEdges (E a b c:e) d f =
  if not (member c d) && a == f 
  then (E a b c:findEdges e d f)
  else findEdges e d f
findEdges Nil d _ = Nil 