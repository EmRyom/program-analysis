module SignAnalysis where



import Data.List (List(..), singleton, (:), nub, concat, nubBy, union, null, intersect)
import ReachingDefinition (defineVariables, defineVariablesStatement, mergeElement)
import Basic (Content(..), Edge(..), Element(..), Sign(..), SignInitialisation, eqElement, name)
import AllTraversals (initAllTraversals)
import ProgramGraph (pgProgram)
import Data.Maybe (Maybe(..))
import Prelude (show, ($), (+), (<>), (==), (>), not, (<))
import AST (AExp(..), LExp(..), Opa(..), Program(..), Statement(..), Opr(..), BExp(..))
import Data.Tuple.Nested ((/\))
import Data.Tuple (Tuple)
import Generator (printSigns)
import Data.Eq
import Data.Set (Set(..), fromFoldable, difference, size)


data SignDetection = AS Int (List AbstractState) 

type AbstractState = Tuple Element (List Sign)  





eqSLAS :: AbstractState -> List AbstractState -> Boolean 
eqSLAS a (b:bs) = if eqAS a b then true else eqSLAS a bs 
eqSLAS _ Nil = false 


eqAS :: AbstractState -> AbstractState -> Boolean
eqAS (c /\ d) (e /\ f) =  if eqElement c e
        then eqLS d f
        else false 

eqLS :: List Sign -> List Sign -> Boolean
eqLS (x:xs) ys = if containSign x ys 
  then eqLS xs ys 
  else false 
eqLS Nil _ = false
    
containSign :: Sign -> List Sign -> Boolean
containSign x (y:ys) = if x == y 
  then true 
  else containSign x ys
containSign _ Nil = false 



saGenerate :: Program -> SignInitialisation -> String
saGenerate p si = let edges = pgProgram p in case p of 
  Program d s -> let elements = nubBy eqElement $ mergeElement (defineVariables d Nil) (defineVariablesStatement s) in
    case checkElements si elements Nil of
        Nil -> "Sign initialisation invalid"
        sd ->  let initialSD = AS 0 sd in printSignDetection $ assemble (concat ((initialSD:Nil):recTraversalSA (initAllTraversals edges) initialSD)) 0 elements


assemble :: List SignDetection -> Int -> List Element -> List SignDetection
assemble allSD a elements = case findSA allSD a of
  Nil -> Nil
  relevantSD -> ((AS a (findElementAS (relevantSD) elements)):assemble allSD (a+1) elements)
    

concatAS :: List SignDetection -> List AbstractState
concatAS ((AS _ a):as) = concat (a:((concatAS as):Nil))
concatAS Nil = Nil

-- unifies AbstractState for all elements in list
findElementAS :: List AbstractState -> List Element -> List AbstractState
findElementAS allAs (e:es) = ((e /\ (nub $ findElement allAs e)):findElementAS allAs es)
findElementAS _ Nil = Nil

findElement :: List AbstractState -> Element -> List Sign
findElement ((a /\ signs):as) e = if eqElement a e then concat (signs:((findElement as e):Nil)) else findElement as e
findElement Nil _ = Nil



recTraversalSA :: List (List Edge) -> SignDetection -> List (List SignDetection)
recTraversalSA (a:as) sd = (recSignAnalysis a sd:recTraversalSA as sd)
recTraversalSA Nil _ = Nil 

findSA :: List SignDetection -> Int -> List AbstractState 
findSA ((AS i a):as) u = if i == u then concat (a:((findSA as u):Nil)) else findSA as u
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
      LArray v z -> if not null $ union (aexpSign z (AS i es)) (Neutral:(Positive:Nil)) 
        then (AS c (addSign es (Array v) (aexpSign y (AS i es))))
        else (AS c es)
      LRfst v -> (AS c (addSign es (Record v) (aexpSign y (AS i es))))
      LRsnd v -> (AS c (addSign es (Record v) (aexpSign y (AS i es))))
    RDef x y z -> (AS c (replaceSign es (Record x) (mergeSigns (aexpSign y (AS i es)) (aexpSign z (AS i es)))))
    Read x -> case x of 
      LVar v -> (AS c (replaceSign es (Var v) allThree))
      LArray v z -> (AS c (replaceSign es (Array v) allThree))
      LRfst v -> (AS c (replaceSign es (Record v) allThree))
      LRsnd v -> (AS c (replaceSign es (Record v) allThree))
    _ -> (AS c es)
  B s -> case s of
    Relational x y z -> relationalAssignment (y) x z (AS c es)
    False -> (AS c Nil)
    --Negation (Relational x y z) -> relationalAssignment (reverseOperator y) x z (AS c es)
    _ -> (AS c es)
  _ -> (AS c es)


allThree :: List Sign
allThree = (Negative:(Neutral:(Positive:Nil)))

numberToSign :: Int -> Sign
numberToSign c = if c>0 then Positive else if c<0 then Negative else Neutral

relationalAssignment :: Opr -> AExp -> AExp -> SignDetection -> SignDetection
relationalAssignment opr a b (AS i es) = case a of
  ANumber c -> case b of 
    AVar d -> (AS i (replaceSign es (Var d) $ intersect (findVar es d) $ (operRelational (revSharpOperator opr)) (numberToSign c)))
    AArray d _ -> (AS i (addSign es (Array d) $ intersect (findVar es d) $ (operRelational (revSharpOperator opr)) (numberToSign c)))
    ARfst d -> (AS i (replaceSign es (Record d) $ intersect (findVar es d) $ (operRelational (revSharpOperator opr)) (numberToSign c)))
    ARsnd d -> (AS i (replaceSign es (Record d) $ intersect (findVar es d) $ (operRelational (revSharpOperator opr)) (numberToSign c)))
    _ -> (AS i es) 
  AVar c -> case b of 
    ANumber d -> (AS i (replaceSign es (Var c) $ intersect (findVar es c) $ (operRelational opr) (numberToSign d)))
    AVar d -> case (AS i (replaceSign es (Var c) $ intersect (findVar es c) $ (allVariableSigns (opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Var d) $ intersect (findVar es d) $ (allVariableSigns (revSharpOperator opr) c (AS i es))))
    AArray d _ -> case (AS i (replaceSign es (Var c) $ intersect (findVar es c) $ (allVariableSigns (opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (addSign es2 (Array d) $ intersect (findVar es d) $ (allVariableSigns (revSharpOperator opr) c (AS i es))))
    ARfst d -> case (AS i (replaceSign es (Var c) $ intersect (findVar es c) $ (allVariableSigns (opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Record d) $ intersect (findVar es d) $ (allVariableSigns (revSharpOperator opr) c (AS i es))))
    ARsnd d -> case (AS i (replaceSign es (Var c) $ intersect (findVar es c) $ (allVariableSigns (opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Record d) $ intersect (findVar es d) $ (allVariableSigns (revSharpOperator opr) c (AS i es))))
    _ -> (AS i es) 
  AArray c _ -> case b of 
    ANumber d -> (AS i (replaceSign es (Array c) $ intersect (findVar es c) $ (operRelational (reverseOperator opr)) (numberToSign d)))
    AVar d -> case (AS i (addSign es (Array c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Var d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    AArray d _ -> case (AS i (addSign es (Array c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (addSign es2 (Array d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    ARfst d -> case (AS i (addSign es (Array c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Record d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    ARsnd d -> case (AS i (addSign es (Array c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Record d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    _ -> (AS i es) 
  ARfst c -> case b of 
    ANumber d -> (AS i (replaceSign es (Record c) $ intersect (findVar es c) $ (operRelational (reverseOperator opr)) (numberToSign d)))
    AVar d -> case (AS i (replaceSign es (Record c) $ intersect (findVar es c) $ (allVariableSigns opr d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Var d) $ intersect (findVar es d) $ (allVariableSigns (reverseOperator opr) c (AS i es))))
    AArray d _ -> case (AS i (addSign es (Record c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Array d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    ARfst d -> case (AS i (addSign es (Record c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Record d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    ARsnd d -> case (AS i (addSign es (Record c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Record d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    _ -> (AS i es) 
  ARsnd c -> case b of 
    ANumber d -> (AS i (replaceSign es (Record c) $ intersect (findVar es c) $ (operRelational (reverseOperator opr)) (numberToSign d)))
    AVar d -> case (AS i (replaceSign es (Record c) $ intersect (findVar es c) $ (allVariableSigns opr d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Var d) $ intersect (findVar es d) $ (allVariableSigns (reverseOperator opr) c (AS i es))))
    AArray d _ -> case (AS i (addSign es (Record c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Array d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    ARfst d -> case (AS i (addSign es (Record c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Record d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    ARsnd d -> case (AS i (addSign es (Record c) $ intersect (findVar es c) $ (allVariableSigns (reverseOperator opr) d (AS i es)))) of 
      (AS i es2) -> (AS i (replaceSign es2 (Record d) $ intersect (findVar es d) $ (allVariableSigns opr c (AS i es))))
    _ -> (AS i es) 
  _ -> (AS i es) 


{-signsFromRelational :: Element -> Int -> Opr -> List Sign
signsFromRelational a i opr = case a of 
  Var b -> 
  Array b -> 
  Record b ->-}


reverseOperator :: Opr -> Opr
reverseOperator c = case c of
  More -> LessEq
  Less -> MoreEq
  MoreEq -> Less
  LessEq -> More
  Eq -> NotEq
  NotEq -> Eq 

revSharpOperator :: Opr -> Opr
revSharpOperator c = case c of
  More -> Less
  Less -> More
  MoreEq -> LessEq
  LessEq -> MoreEq 
  Eq -> Eq
  NotEq -> NotEq 


operRelational :: Opr -> (Sign -> List Sign)
operRelational a = case a of
   More -> operMore 
   Less -> operLess
   MoreEq -> operMoreEq
   LessEq -> operLessEq
   Eq     -> operEq
   NotEq  -> operNotEq


allOperSign :: Opr -> List Sign -> List (List Sign)
allOperSign opr (a:as) = (operRelational opr a:allOperSign opr as)
allOperSign opr Nil = Nil


allVariableSigns :: Opr -> String -> SignDetection -> List Sign
allVariableSigns opr el (AS i es) =
  let initSigns = findVar es el in
  nub $ concat (allOperSign opr initSigns)


-- What do these operations say about the leftmost operand?
operMore :: Sign -> List Sign 
operMore Positive = (Positive:Nil)
operMore Neutral =  (Positive:Nil)
operMore Negative = (Positive:(Neutral:(Negative:Nil)))

operLess :: Sign -> List Sign 
operLess Positive = (Positive:(Neutral:(Negative:Nil)))
operLess Neutral =  (Negative:Nil)
operLess Negative = (Negative:Nil)

operMoreEq :: Sign -> List Sign 
operMoreEq Positive = (Positive:Nil)
operMoreEq Neutral  = (Positive:(Neutral:Nil))
operMoreEq Negative = (Positive:(Neutral:(Negative:Nil)))

operLessEq :: Sign -> List Sign 
operLessEq Positive = (Positive:(Neutral:(Negative:Nil)))
operLessEq Neutral  = (Neutral:(Negative:Nil))
operLessEq Negative = (Negative:Nil)

operEq :: Sign -> List Sign 
operEq a = (a:Nil)

operNotEq :: Sign -> List Sign 
operNotEq Positive = (Neutral:(Negative:Nil))
operNotEq Neutral  = (Positive:(Negative:Nil))
operNotEq Negative = (Positive:(Neutral:Nil))


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
  AArray a z -> if not null $ union (aexpSign z (AS x es)) (Neutral:(Positive:Nil)) 
    then findVar es a
    else Nil
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