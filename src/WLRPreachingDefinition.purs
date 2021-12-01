module WLRPreachingDefinition where


import Worklist (Worklist, extract, insert, empty, worklistQueue, worklistStack)
import Data.CatList (CatList)
import Data.Maybe (Maybe(..))
import Basic (Edge(..), Element, eqElement)
import Prelude (show, ($), (+), (<>), (==))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import ProgramGraph (highest, pgProgram)
import AST (Program(..))
import Data.List (List(..), (:), nubBy, concat)
import ReachingDefinition (Assignment, ReachingDefinition(..), defineVariables, defineVariablesStatement, eqAssignment, mergeAssignment, mergeElement, printReachingDefinitions, solveConstraint, unknownDefinition)
import ReversePostorder 



rdRPWorklist :: Program -> String
rdRPWorklist p = printReachingDefinitions (worklistRD p) <> """
""" <> pl p


pl :: Program -> String
pl p = let edges = pgProgram p in case depthFirstSpanning edges of
    (_ /\ a) -> printList a 

printList :: List Int -> String
printList (a:as) = show a <> " " <> printList as
printList Nil = ""


worklistRD :: Program -> List ReachingDefinition
worklistRD p = let edges = pgProgram p in case p of
  Program d s -> let elements = nubBy eqElement $ mergeElement (defineVariables d Nil) (defineVariablesStatement s) in
    initWLRD edges elements

initWLRD :: List Edge -> List Element -> List ReachingDefinition
initWLRD edges elements = case depthFirstSpanning edges of
    (_ /\ a) -> let worklist = worklistStack a empty in
        let dsquare = (unknownDefinition elements 0:initMemory edges) in 
        recWLRD edges worklist dsquare
  
initMemory :: List Edge -> List ReachingDefinition 
initMemory edges = let end = highest edges 0 in recMemory 1 end

initAsQueue :: List Edge -> List Int
initAsQueue edges = let high = highest edges 0 in rangeQueue 0 high

rangeQueue :: Int -> Int -> List Int
rangeQueue a b = if a == b then (a:Nil) else (a:rangeQueue (a+1) b)


recMemory :: Int -> Int -> List ReachingDefinition
recMemory a b = if a == b then (RD a Nil:Nil) else (RD a Nil:recMemory (a+1) b)

recWLRD :: List Edge -> Worklist -> List ReachingDefinition -> List ReachingDefinition
recWLRD edges worklist rd = 
  case extract worklist of
    Nothing -> rd
    Just (newNode /\ newWorklist) -> case forEveryRD newWorklist rd (recEdges edges rd newNode) of 
      (newNewWorklist /\ newRD) -> recWLRD edges newNewWorklist newRD

forEveryRD :: Worklist -> List ReachingDefinition -> List ReachingDefinition -> Tuple Worklist (List ReachingDefinition)
forEveryRD worklist rd (RD a b:as) = 
  let newWorklist = insert worklist a true in
  let replacedRD = replaceRD rd (RD a b) in 
  forEveryRD newWorklist replacedRD as 
forEveryRD worklist rd Nil = (worklist /\ rd)



replaceRD :: List ReachingDefinition -> ReachingDefinition -> List ReachingDefinition
replaceRD (RD a as:bs) (RD c cs) = if a == c then (RD c cs:bs) else (RD a as:replaceRD bs (RD c cs))
replaceRD Nil (RD c cs) = (RD c cs:Nil)



recEdges :: List Edge -> List ReachingDefinition -> Int -> List ReachingDefinition
recEdges edges rd node = 
  let allOutEdges = findAllEdges edges node in
  forAllEdges allOutEdges rd


forAllEdges :: List Edge -> List ReachingDefinition -> List ReachingDefinition
forAllEdges (e:es) rd = concat ((forOneEdge e rd):((forAllEdges es rd):Nil))
forAllEdges Nil rd = Nil


forOneEdge :: Edge -> List ReachingDefinition -> List ReachingDefinition
forOneEdge (E a b c) rd = 
  let reachingDef = findRD a rd in
  let oldRD = findRD c rd in
  let newRD = solveConstraint reachingDef (E a b c) in
  let newMergedRD = mergeRD oldRD newRD in
  case oldRD of 
    (RD x xs) -> case newMergedRD of 
      (RD y ys) -> if isSubset xs ys 
                    then Nil
                    else (newMergedRD:Nil)
  

isMember :: List Assignment -> Assignment -> Boolean
isMember (x:xs) f = if eqAssignment f x then true else isMember xs f
isMember _ f = false

isSubset :: List Assignment -> List Assignment -> Boolean
isSubset o (n:ns) = if isMember o n 
  then isSubset o ns
  else false
isSubset o n = listnull n

listnull :: List Assignment -> Boolean
listnull Nil = true
listnull _ = false

mergeRD :: ReachingDefinition -> ReachingDefinition -> ReachingDefinition
mergeRD (RD x as) (RD y bs) = (RD x (nubBy eqAssignment $ mergeAssignment as bs))




findRD :: Int -> List ReachingDefinition -> ReachingDefinition  
findRD a (RD b es:bs) = 
  if a == b 
  then (RD b es)
  else findRD a bs
findRD a Nil = RD a Nil

findAllEdges :: List Edge -> Int -> List Edge
findAllEdges (E a b c:es) node =
  if node == a 
  then (E a b c:findAllEdges es node)
  else findAllEdges es node
findAllEdges Nil node = Nil