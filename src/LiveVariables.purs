module LiveVariables where 


import AST
import ProgramGraph (pgProgram, highest)
import ReachingDefinition (defineVariables, defineVariablesStatement, fvAExp, fvBExp)
import AllTraversals (initAllTraversals)
import Data.List (List(..), concat, nubBy, singleton, (:), deleteBy, unionBy, reverse)
import Prelude (negate, show, ($), (&&), (-), (<>), (==))
import Basic (Element(..), eqElement, Edge(..), Content(..))


lvGenerate :: Program -> String
lvGenerate p = let edges = pgProgram p in case p of 
  Program d s -> printLiveVariables $ reverse (initLV edges)


reverseEdges :: List Edge -> List Edge
reverseEdges (E a b c:as) = (E c b a:reverseEdges as)
reverseEdges Nil = Nil 


initLV :: List Edge -> List LiveVariable
initLV edges = 
  let limit = highest edges 0 in 
  assemble (concat (recLV (initAllTraversals edges) (LV limit Nil))) limit

assemble :: List LiveVariable -> Int -> List LiveVariable
assemble as -1 = Nil 
assemble as i =
  case nubBy eqElement (findLV as i) of
    Nil -> (LV i Nil:assemble as (i-1))
    r -> (LV i r:assemble as (i-1))

 

findLV :: List LiveVariable -> Int -> List Element 
findLV (LV i as:s) u = if i == u then mergeElement as (findLV s u) else findLV s u
findLV Nil _ = Nil 

mergeElement :: List Element -> List Element -> List Element
mergeElement (a:as) b = mergeElement as (a:b)
mergeElement Nil b = b


recLV :: List (List Edge) -> LiveVariable -> List (List LiveVariable)
recLV (a:as) r = ((liveVariable (reverse $ reverseEdges a) r):recLV as r)
recLV Nil r = Nil 


liveVariable :: List Edge -> LiveVariable -> List LiveVariable 
liveVariable (a:as) b = 
  let newLV = solveConstraint b a in
  (newLV:liveVariable as newLV)
liveVariable Nil _ = Nil

solveConstraint :: LiveVariable -> Edge -> LiveVariable
solveConstraint (LV i a) edge = 
  case constraint edge of 
    (C _ o k g) -> case k of 
      Kill e -> let newAl = deleteBy eqElement e a in 
        case g of 
          Gen x -> LV o (unionBy eqElement x newAl)
          NoGen -> LV o newAl
      NoKill -> case g of 
        Gen x -> LV o (unionBy eqElement x a)
        NoGen -> LV o a 

constraint :: Edge -> Constraint
constraint (E x y z) = case y of
  D a -> case a of 
      DVar b -> C x z (Kill (Var b)) NoGen 
      DArray b c -> C x z (Kill (Array b)) $ NoGen
      DRecord b -> C x z (Kill (Record b)) $ NoGen 
      _ -> C x z NoKill NoGen
  S a -> case a of 
      LDef b c -> case b of 
        LVar o -> C x z (Kill (Var o)) $ Gen (fvAExp c)
        LArray o m -> C x z (NoKill) $ Gen (unionBy eqElement (fvAExp m) (fvAExp c))
        LRfst o ->  C x z (Kill (Record o)) $ Gen (fvAExp c)
        LRsnd o ->  C x z (Kill (Record o)) $ Gen (fvAExp c)
      RDef b c d -> C x z (Kill (Record b)) $ Gen (unionBy eqElement (fvAExp d) (fvAExp c)) 
      Read b -> case b of 
        LVar o ->  C x z (Kill (Var o)) $ NoGen
        LArray o c ->  C x z (NoKill) $ Gen (fvAExp c)
        LRfst o -> C x z (Kill (Record o)) $ NoGen
        LRsnd o -> C x z (Kill (Record o)) $ NoGen
      Write b -> C x z NoKill $ Gen (fvAExp b)
      _ -> C x z NoKill NoGen
  B a -> C x z NoKill $ Gen (fvBExp a) 


data LiveVariable = LV Int (List Element)
data Constraint         = C Int Int Killset Genset
                         -- Out|In|Kill|Gen
data Genset 
  = Gen (List Element)  
  | NoGen
data Killset 
  = Kill Element
  | NoKill


printLiveVariables :: List (LiveVariable) -> String
printLiveVariables (LV a b:as) = "LV(q" <> show a <> ")={" <> printLV b <> """}
""" <> printLiveVariables as 
printLiveVariables Nil = ""

printLV :: List (Element) -> String 
printLV (a:Nil) = case a of 
  Var b -> b
  Record b -> b
  Array b -> b
printLV (Var a:as) = a <> "," <> printLV as
printLV (Record a:as) = a <> "," <> printLV as
printLV (Array a:as) = a <> "," <> printLV as
printLV Nil = ""

