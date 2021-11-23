module ReachingDefinition where

import AST
import ProgramGraph (pgProgram)
import Basic
import AllTraversals (initAllTraversals)
import Data.List (List(..), concat, nubBy, singleton, (:))
import Prelude (negate, show, ($), (&&), (+), (<>), (==))

rdGenerate :: Program -> String
rdGenerate p = let edges = pgProgram p in case p of 
  Program d s -> let els = nubBy eqElement $ mergeElement (defineVariables d Nil) (defineVariablesAssignment s) in
    printReachingDefinitions (initRD edges els)


initRD :: List Edge -> List Element -> List ReachingDefinition
initRD edges elements = 
  let firstRD = unknownDefinition elements 0 in
  assemble (firstRD:concat (recRD (initAllTraversals edges) firstRD)) 0 

assemble :: List ReachingDefinition -> Int -> List ReachingDefinition
assemble as i =
  case nubBy eqAssignment (findRD as i) of
    Nil -> Nil
    r -> (RD i r:assemble as (i+1))

 

findRD :: List ReachingDefinition -> Int -> List Assignment 
findRD (RD i as:s) u = if i == u then mergeAssignment as (findRD s u) else findRD s u
findRD Nil _ = Nil 

mergeAssignment :: List Assignment -> List Assignment -> List Assignment
mergeAssignment (a:as) b = mergeAssignment as (a:b)
mergeAssignment Nil b = b


recRD :: List (List Edge) -> ReachingDefinition -> List (List ReachingDefinition)
recRD (a:as) r = (reachingDefinition a r:recRD as r)
recRD Nil r = Nil 


reachingDefinition :: List Edge -> ReachingDefinition -> List ReachingDefinition 
reachingDefinition (a:as) b = 
  let newRD = solveConstraint b a in
  (newRD:reachingDefinition as newRD)
reachingDefinition Nil _ = Nil


eqAssignment :: Assignment -> Assignment -> Boolean 
eqAssignment (A a b c) (A d e f) = eqElement a d && b == e && c == f

eqConstraint :: Constraint -> Constraint -> Boolean
eqConstraint (C a b c d) (C e f g h) = a == e && b == f && eqKill c g && eqGen d h

eqKill :: Killset -> Killset -> Boolean
eqKill (Kill a) (Kill b) = eqElement a b
eqKill NoKill NoKill = true 
eqKill _ _ = false

eqGen :: Genset -> Genset -> Boolean
eqGen (Gen a b c) (Gen d e f) = eqElement a d && b == e && c == f
eqGen NoGen NoGen = true
eqGen _ _ = false 

eqElement :: Element -> Element -> Boolean
eqElement (Var a) (Var b) = a == b
eqElement (Array a) (Array b) = a == b
eqElement (Record a) (Record b) = a == b
eqElement _ _ = false 

solveConstraint :: ReachingDefinition -> Edge -> ReachingDefinition
solveConstraint (RD i a) edge = 
  case constraint edge of 
    (C _ o k g) -> case k of 
      Kill e -> let newAl = removeElement a e in 
        case g of 
          Gen x y z -> RD o (A x y z:newAl)
          NoGen -> RD o newAl
      NoKill -> case g of 
        Gen x y z -> RD o (A x y z:a)
        NoGen -> RD o a 

removeElement :: List Assignment -> Element -> List Assignment
removeElement (A a b c:as) d = 
  if name a == name d
  then removeElement as d
  else (A a b c:removeElement as d)
removeElement Nil _ = Nil 

constraint :: Edge -> Constraint
constraint (E x y z) = case y of
  D a -> case a of 
      DVar b -> C x z (Kill (Var b)) (Gen (Var b) x z)
      DArray b c -> C x z (Kill (Array b)) $ Gen (Array b) x z
      DRecord b -> C x z (Kill (Record b)) $ Gen (Record b) x z 
      _ -> C x z NoKill NoGen
  S a -> case a of 
      LDef b c -> case b of 
        LVar o -> C x z (Kill (Var o)) $ Gen (Var o) x z
        LArray o _ -> C x z (Kill (Array o)) $ Gen (Array o) x z
        LRfst o ->  C x z (Kill (Record o)) $ Gen (Record o) x z
        LRsnd o ->  C x z (Kill (Record o)) $ Gen (Record o) x z
      RDef b c d -> C x z (Kill (Record b)) $ Gen (Record b) x z
      Read b -> case b of 
        LVar o ->  C x z (Kill (Var o)) $ Gen (Var o) x z
        LArray o _ ->  C x z NoKill $ Gen (Array o) x z
        LRfst o -> C x z (Kill (Record o)) $ Gen (Record o) x z
        LRsnd o -> C x z (Kill (Record o)) $ Gen (Record o) x z
      Write b -> C x z NoKill NoGen
      _ -> C x z NoKill NoGen
  B a -> C x z NoKill NoGen
   
unknownDefinition :: List Element -> Int -> ReachingDefinition
unknownDefinition e d = (RD d (ud e d))

ud :: List Element -> Int -> List Assignment
ud (e:es) d = (A e (-1) d:ud es d)
ud Nil _ = Nil


data ReachingDefinition = RD Int (List Assignment)
                          -- Node|Reaching Definition
data Assignment         = A Element Int Int
                         -- Var/Arr|Out|In
data Constraint         = C Int Int Killset Genset
                         -- Out|In|Kill|Gen
data Element 
  = Var String 
  | Array String 
  | Record String 
data Genset 
  = Gen Element Int Int 
  | NoGen
data Killset 
  = Kill Element
  | NoKill


mergeElement :: List Element -> List Element -> List Element
mergeElement (a:as) b = mergeElement as (a:b)
mergeElement Nil b = b

defineVariables :: Declaration -> List Element -> List Element
defineVariables d le = case d of 
  DVar s -> replaceElementName (Var s) le 
  DArray s i -> replaceElementName (Array s) le 
  DRecord s -> replaceElementName (Record s) le  
  None -> le 
  DDouble d1 d2 -> defineVariables d2 $ defineVariables d1 le 

defineVariablesAssignment :: Statement -> List Element 
defineVariablesAssignment s = case s of 
  LDef a b -> case a of
    LVar o -> (Var o:Nil)
    LArray o _ -> (Array o:Nil)
    LRfst o -> (Record o:Nil)
    LRsnd o -> (Record o:Nil)
  RDef a _ _ -> (Record a:Nil)
  SDouble a b -> mergeElement (defineVariablesAssignment a) (defineVariablesAssignment b) 
  If a b -> (defineVariablesAssignment b)
  Ifelse a b c -> mergeElement (defineVariablesAssignment b) (defineVariablesAssignment c)
  While a b -> (defineVariablesAssignment b)
  Read a -> case a of
    LVar o -> (Var o:Nil)
    LArray o _ -> (Array o:Nil)
    LRfst o -> (Record o:Nil)
    LRsnd o -> (Record o:Nil)
  Write a -> Nil

defineVariablesStatement :: Statement -> List Element 
defineVariablesStatement s = case s of 
  LDef a b -> mergeElement (fvLExp a) (fvAExp b) 
  RDef a b c -> mergeElement (Record a:fvAExp b) $ fvAExp c
  SDouble a b -> mergeElement (defineVariablesStatement a) (defineVariablesStatement b) 
  If a b -> mergeElement (defineVariablesStatement b) (fvBExp a)
  Ifelse a b c -> mergeElement (defineVariablesStatement b) (mergeElement (fvBExp a) (defineVariablesStatement c))
  While a b -> mergeElement (defineVariablesStatement b) (fvBExp a)
  Read a -> fvLExp a
  Write a -> fvAExp a 

replaceElement :: Element -> List Element -> List Element
replaceElement e (a:as) =
  if eqElement a e 
  then replaceElement e as
  else (a:replaceElement e as)
replaceElement e Nil = singleton e 

fvAExp :: AExp -> List Element 
fvAExp (AArray x b) = (Array x:fvAExp b)
fvAExp (ARfst x) = singleton (Record x)
fvAExp (ARsnd x) = singleton (Record x)
fvAExp (Arithmetic a _ c) = concat (fvAExp a:(singleton $ fvAExp c))
fvAExp (AVar x) = singleton (Var x)
fvAExp (ANumber x) = Nil

fvBExp :: BExp -> List Element
fvBExp (Relational a _ c) = concat (fvAExp a:singleton (fvAExp c))
fvBExp (Boolean a _ c) = concat (fvBExp a:singleton (fvBExp c))
fvBExp (Negation a) = fvBExp a 
fvBExp _ = Nil 

fvLExp :: LExp -> List Element 
fvLExp (LVar a) = (Var a:Nil)
fvLExp (LArray a b) = (Array a:fvAExp b)
fvLExp (LRfst a) = (Record a:Nil)
fvLExp (LRsnd a) = (Record a:Nil)

replaceElementName :: Element -> List Element -> List Element
replaceElementName e (a:as) =
  if name a == name e 
  then replaceElementName e as
  else (a:replaceElementName e as)
replaceElementName e Nil = singleton e 

name :: Element -> String
name (Var a) = a
name (Array a) = a
name (Record a) = a

printReachingDefinitions :: List (ReachingDefinition) -> String
printReachingDefinitions (RD a b:as) = "RD(q" <> show a <> ")={" <> printRD b <> """}
""" <> printReachingDefinitions as 
printReachingDefinitions Nil = ""

printRD :: List (Assignment) -> String 
printRD (a:as) = "(" <> strRD a <> ")" <> printRD as
printRD Nil = ""

strRD :: Assignment -> String 
strRD (A e a b) = let r = "," <> (if a == -1 then "?" else show a) <> "," <> show b in
  case e of 
    Var f -> f <> r
    Array f -> f <> r
    Record f -> f <> r
