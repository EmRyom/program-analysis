module LiveVariables where 


import AST
import ProgramGraph (Content(..), Edge(..), pgProgram, highest)
import AllTraversals (initAllTraversals)
import Data.List (List(..), concat, nubBy, singleton, (:),deleteBy, unionBy)
import Prelude (negate, show, ($), (&&), (-), (<>), (==))

lvGenerate :: Program -> String
lvGenerate p = let edges = pgProgram p in case p of 
  Program d s -> printLiveVariables (initLV edges (defineVariables d Nil))


reverseEdges :: List Edge -> List Edge
reverseEdges (E a b c:as) = (E c b a:reverseEdges as)
reverseEdges Nil = Nil 


initLV :: List Edge -> List Element -> List LiveVariable
initLV edges elements = 
  let limit = highest edges 0 in 
  assemble (concat (recLV (initAllTraversals edges) (LV limit elements))) limit

assemble :: List LiveVariable -> Int -> List LiveVariable
assemble as 0 = Nil 
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
recLV (a:as) r = (liveVariable (reverseEdges a) r:recLV as r)
recLV Nil r = Nil 


liveVariable :: List Edge -> LiveVariable -> List LiveVariable 
liveVariable (a:as) b = 
  let newLV = solveConstraint b a in
  (newLV:liveVariable as newLV)
liveVariable Nil _ = Nil





eqKill :: Killset -> Killset -> Boolean
eqKill (Kill a) (Kill b) = eqElement a b
eqKill NoKill NoKill = true 
eqKill _ _ = false


eqElement :: Element -> Element -> Boolean
eqElement (Var a) (Var b) = a == b
eqElement (Array a) (Array b) = a == b
eqElement (Record a) (Record b) = a == b
eqElement _ _ = false 

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

    



data LiveVariable = LV Int (List Element)
                          -- Node|Reaching Definition
data Constraint         = C Int Int Killset Genset
                         -- Out|In|Kill|Gen
data Element 
  = Var String 
  | Array String 
  | Record String 
data Genset 
  = Gen (List Element)  
  | NoGen
data Killset 
  = Kill Element
  | NoKill


defineVariables :: Declaration -> List Element -> List Element
defineVariables d le = case d of 
  DVar s -> replaceElement (Var s) le 
  DArray s i -> replaceElement (Array s) le 
  DRecord s -> replaceElement (Record s) le  
  None -> le 
  DDouble d1 d2 -> defineVariables d2 $ defineVariables d1 le 

replaceElement :: Element -> List Element -> List Element
replaceElement e (a:as) =
  if name a == name e 
  then replaceElement e as
  else (a:replaceElement e as)
replaceElement e Nil = singleton e 

name :: Element -> String
name (Var a) = a
name (Array a) = a
name (Record a) = a

printLiveVariables :: List (LiveVariable) -> String
printLiveVariables (LV a b:as) = "LV(q" <> show a <> ")={" <> printLV b <> """}
""" <> printLiveVariables as 
printLiveVariables Nil = ""

printLV :: List (Element) -> String 
printLV (Var a:as) = "(" <> a <> ")" <> printLV as
printLV (Record a:as) = "(" <> a <> ")" <> printLV as
printLV (Array a:as) = "(" <> a <> ")" <> printLV as
printLV Nil = ""

