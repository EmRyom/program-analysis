module WLsignAnalysis where



import SignAnalysis
import Data.List (List(..), (:), nubBy, concat)
import Prelude
import ProgramGraph 
import Basic 
import Worklist
import AST
import Data.Tuple 
import Data.Tuple.Nested ((/\))
import Data.Maybe
import ReachingDefinition





saWorklist :: Program -> SignInitialisation -> List SignDetection
saWorklist p si = let edges = pgProgram p in case p of
  Program d s -> let elements = nubBy eqElement $ mergeElement (defineVariables d Nil) (defineVariablesStatement s) in
    case checkElements si elements Nil of
        Nil -> "Sign initialisation invalid"
        sd -> let dsquare = ((AS 0 sd):initMemory edges) in
            let worklist = worklistQueue (initAsQueue edges) empty in
                recWLSA edges worklist dsquare 
  
initMemory :: List Edge -> List SignDetection 
initMemory edges = let end = highest edges 0 in recMemory 1 end

recMemory :: Int -> Int -> List SignDetection    
recMemory a b = if a == b then ((AS a Nil):Nil) else ((AS a Nil):recMemory (a+1) b)

recWLSA :: List Edge -> Worklist -> List SignDetection -> List SignDetection
recWLSA edges worklist sd = 
  case extract worklist of
    Nothing -> sd
    Just (newNode /\ newWorklist) -> case forEverySD newWorklist sd (recEdges edges sd newNode) of 
      (newNewWorklist /\ newSD) -> recWLSA edges newNewWorklist newSD

forEverySD :: Worklist -> List SignDetection -> List SignDetection -> Tuple Worklist (List SignDetection)
forEverySD worklist sd ((AS a b):as) = 
  let newWorklist = insert worklist a false in
  let replacedSD = replaceSD sd (AS a b) in 
  forEverySD newWorklist replacedSD as 
forEverySD worklist sd Nil = (worklist /\ sd)



replaceSD :: List SignDetection -> SignDetection -> List SignDetection
replaceSD ((AS a as):bs) (AS c cs) = if a == c then ((AS c cs):bs) else ((AS a as):replaceSD bs (AS c cs))
replaceSD Nil (AS c cs) = ((AS c cs):Nil)



recEdges :: List Edge -> List SignDetection -> Int -> List SignDetection
recEdges edges sd node = 
  let allOutEdges = findAllEdges edges node in
  forAllEdges allOutEdges sd


forAllEdges :: List Edge -> List SignDetection -> List SignDetection
forAllEdges (e:es) sd = concat ((forOneEdge e sd):((forAllEdges es sd):Nil))
forAllEdges Nil sd = Nil


forOneEdge :: Edge -> List SignDetection -> List SignDetection
forOneEdge (E a b c) sd = 
  let reachingDef = findSD a sd in
  let oldSD = findSD c sd in
  let newSD = signAnalysis reachingDef (E a b c) in
  let newMergedSD = mergeSD oldSD newSD in
  case oldSD of 
    (AS x xs) -> case newMergedSD of 
      (AS y ys) -> if isSubset xs ys 
                   then Nil
                   else (newMergedSD:Nil)
  

isSubset :: List AbstractState -> List AbstractState -> Boolean
isSubset (a:as) b = if eqSLAS a b then eqSLAS as b else false
isSubset Nil _ = true 




listnull :: List AbstractState -> Boolean
listnull Nil = true
listnull _ = false

mergeSD :: SignDetection -> SignDetection -> SignDetection
mergeSD (AS x as) (AS y bs) = (AS x (findElementAS as bs))



findSD :: Int -> List SignDetection -> SignDetection  
findSD a ((AS b es):bs) = 
  if a == b 
  then (AS b es)
  else findSD a bs
findSD a Nil = AS a Nil

findAllEdges :: List Edge -> Int -> List Edge
findAllEdges (E a b c:es) node =
  if node == a 
  then (E a b c:findAllEdges es node)
  else findAllEdges es node
findAllEdges Nil node = Nil