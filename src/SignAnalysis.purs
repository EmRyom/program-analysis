module SignAnalysis where



import Data.List (List(..), singleton, (:), nub, concat, nubBy)
import ReachingDefinition
import Basic 
import AllTraversals
import ProgramGraph (pgProgram)
import Data.Maybe (Maybe(..))
import Prelude
import AST
import Data.Tuple.Nested ((/\))
import Data.Tuple
import Generator (printSigns)


data SignDetection = AS Int (List AbstractState) 

type AbstractState = Tuple Element (List Sign)  



saGenerate :: Program -> SignInitialisation -> String
saGenerate p si = let edges = pgProgram p in case p of 
  Program d s -> let elements = nubBy eqElement $ mergeElement (defineVariables d Nil) (defineVariablesStatement s) in
    case checkElements si elements Nil of
        Nil -> "Sign initialisation invalid"
        sd ->  let initialSD = AS 0 sd in printSignDetection $ assemble (concat ((initialSD:Nil):recTraversalSA (initAllTraversals edges) initialSD)) 0 elements


assemble :: List SignDetection -> Int -> List Element -> List SignDetection
assemble allSD a elements = case findSA allSD a of
  Nil -> Nil
  relevantSD -> (AS a (findElementAS (concatAS relevantSD) elements):assemble allSD (a+1) elements)
    

concatAS :: List SignDetection -> List AbstractState
concatAS (AS _ a:as) = (a:concatAS as)
concatAS Nil = Nil

-- unifies AbstractState for all elements in list
findElementAS :: List AbstractState -> List Element -> List AbstractState
findElementAS allAs (e:es) = ((e /\ (nub $ findElement allAs e)):findElementAS allAs es)
findElementAS _ Nil = Nil

findElement :: List AbstractState -> Element -> List Sign
findElement ((a /\ signs):as) e = if eqElement a e then (signs:findElement as e) else findElement as e
findElement Nil _ = Nil



recTraversalSA :: List (List Edge) -> SignDetection -> List (List SignDetection)
recTraversalSA (a:as) sd = (recSignAnalysis a sd:recTraversalSA as sd)
recTraversalSA Nil _ = Nil 


findSSA :: List SignDetection -> Int -> SignDetection 
findSSA (AS i a:as) u = AS i (findSA (AS i a:as) u)
findSSA Nil _ = Nil 

findSA :: List SignDetection -> Int -> List AbstractState 
findSA (AS i a:as) u = if i == u then concat (a:(findSA as u:Nil)) else findSA as u
findSA Nil _ = Nil

recSignAnalysis :: List Edge -> SignDetection -> List SignDetection
recSignAnalysis (e:es) sd = let newSD = signAnalysis sd e in 
    (newSD:recSignAnalysis es newSD)
recSignAnalysis Nil _ = Nil
      

checkElements :: SignInitialisation -> List Element -> List AbstractState -> List AbstractState 
checkElements a (e:es) xs = case checkElement a e of 
    Just x -> checkElements a es (x:xs)
    Nothing -> Nil
checkElements a Nil xs = xs  

checkElement :: SignInitialisation -> Element -> Maybe AbstractState
checkElement ((a /\ as):bs) e = if name e == a then Just (e /\ as) else checkElement bs e
checkElement Nil a = Nothing 

signAnalysis :: SignDetection -> Edge -> SignDetection
signAnalysis (AS i es) (E _ b c) = case b of 
  S s -> case s of 
    LDef x y -> case x of 
      LVar v -> (AS c (replaceSign es (Var v) (aexpSign y (AS i es))))
      LArray v _ -> (AS c (addSign es (Array v) (aexpSign y (AS i es))))
      LRfst v -> (AS c (addSign es (Record v) (aexpSign y (AS i es))))
      LRsnd v -> (AS c (addSign es (Record v) (aexpSign y (AS i es))))
    RDef x y z -> (AS c (addSign es (Record x) (mergeSigns (aexpSign y (AS i es)) (aexpSign z (AS i es)))))
    Read x -> case x of 
      LVar v -> (AS c (replaceSign es (Var v) allThree))
      LArray v _ -> (AS c (replaceSign es (Array v) allThree))
      LRfst v -> (AS c (replaceSign es (Record v) allThree))
      LRsnd v -> (AS c (replaceSign es (Record v) allThree))
    _ -> (AS c es)
  _ -> (AS c es)

allThree :: List Sign
allThree = (Negative:(Neutral:(Positive:Nil)))

replaceSign :: List AbstractState -> Element -> List Sign -> List AbstractState
replaceSign ((i /\ e):es) a ne = if eqElement i a then ((i /\ ne):es) else ((i /\ e):replaceSign es a ne)
replaceSign Nil _ _ = Nil 

addSign :: List AbstractState -> Element -> List Sign -> List AbstractState
addSign ((i /\ e):es) a ne = if eqElement i a then ((i /\ (mergeSigns e ne)):es) else ((i /\ e):replaceSign es a ne)
addSign Nil _ _ = Nil 


aexpSign :: AExp -> SignDetection -> List Sign
aexpSign l (AS x es) = case l of 
  ANumber a -> singleton (if a > 0 then Positive else
                         if 0 > a then Negative else Neutral)
  AVar a -> findVar es a
  AArray a _ -> findVar es a
  ARfst a -> findVar es a
  ARsnd a -> findVar es a
  Arithmetic a op b -> recOps op (aexpSign a (AS x es)) (aexpSign b (AS x es)) Nil

findVar :: List AbstractState -> String -> List Sign
findVar ((e /\ a):es) b = if name e == b then a else findVar es b
findVar Nil b = Nil

recOps :: Opa -> List Sign -> List Sign -> List Sign -> List Sign
recOps o (a:as) b result = case recOp o a b Nil of
  Nil -> Nil 
  c -> (recOps o as b (mergeSigns c result)) 
recOps _ Nil _ result = result

recOp :: Opa -> Sign -> List Sign -> List Sign -> List Sign 
recOp o a (b:bs) result = case o of 
  Addition -> recOp o a bs (mergeSigns (operAddition a b) (result))
  Substraction -> recOp o a bs (mergeSigns (operSubstraction a b) (result))
  Multiplication -> recOp o a bs (mergeSigns (operMultiplication a b) (result))
  Division -> case operDivision a b of
    Nil -> Nil
    c -> recOp o a bs (mergeSigns c result)
  Remainder -> case operRemainder a b of
    Nil -> Nil
    c -> recOp o a bs (mergeSigns c result)
recOp _ _ Nil result = result 

mergeSigns :: List Sign -> List Sign -> List Sign
mergeSigns a b = nub $ concat (a:(b:Nil)) 

operAddition :: Sign -> Sign -> List Sign 
operAddition a b = case a of 
    Negative -> case b of 
      Negative -> singleton Negative 
      Neutral -> singleton Negative
      Positive -> (Negative:(Neutral:(Positive:Nil))) 
    Neutral -> case b of 
      Negative -> singleton Negative
      Neutral -> singleton Neutral
      Positive -> singleton Positive
    Positive -> case b of 
      Negative -> (Negative:(Neutral:(Positive:Nil))) 
      Neutral -> singleton Positive
      Positive -> singleton Positive

operSubstraction :: Sign -> Sign -> List Sign 
operSubstraction a b = case a of 
    Negative -> case b of 
      Negative -> (Negative:(Neutral:(Positive:Nil))) 
      Neutral -> singleton Negative
      Positive -> singleton Negative
    Neutral -> case b of 
      Negative -> singleton Positive
      Neutral -> singleton Neutral
      Positive -> singleton Negative
    Positive -> case b of 
      Negative -> singleton Positive
      Neutral -> singleton Positive
      Positive -> (Negative:(Neutral:(Positive:Nil))) 

operMultiplication :: Sign -> Sign -> List Sign 
operMultiplication a b = case a of 
    Negative -> case b of 
      Negative -> singleton Positive 
      Neutral -> singleton Neutral
      Positive -> singleton Negative
    Neutral -> singleton Neutral
    Positive -> case b of 
      Negative -> singleton Negative
      Neutral -> singleton Neutral
      Positive -> singleton Positive

operDivision :: Sign -> Sign -> List Sign 
operDivision a b = case b of 
    Negative -> case a of 
      Negative -> singleton Positive 
      Neutral -> singleton Neutral 
      Positive -> singleton Negative
    Neutral -> Nil
    Positive -> case b of 
      Negative -> singleton Negative
      Neutral -> singleton Neutral
      Positive -> singleton Positive

operRemainder :: Sign -> Sign -> List Sign 
operRemainder a b = case b of 
    Negative -> case a of 
      Negative -> (Negative:(Neutral:Nil))
      Neutral -> singleton Neutral 
      Positive -> (Negative:(Neutral:Nil))
    Neutral -> Nil
    Positive -> case b of 
      Negative -> (Neutral:(Positive:Nil))
      Neutral -> singleton Neutral
      Positive -> (Neutral:(Positive:Nil))



printSignDetection :: List SignDetection -> String
printSignDetection ((AS i es):as) = "AS(q" <> show i <> ")=  " <> printAS es <> """
""" <> printSignDetection as
printSignDetection Nil = ""

printAS :: List AbstractState -> String
printAS ((e /\ es):bs) = (case e of 
    Var i -> i
    Record i -> i
    Array i -> i) <> " -> {" <> printSigns es <> "}   " <> printAS bs
printAS Nil = ""