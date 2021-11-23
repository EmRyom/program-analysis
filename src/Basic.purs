module Basic where 


import Data.List (List(..), (:))
import AST (AExp(..), BExp(..), Declaration(..), LExp(..), Opa(..), Opb(..), Opr(..), Statement(..))
import Prelude ((&&), (==))
import Data.Ord 
import Data.Eq


data Edge = E Int Content Int

instance ordEdge :: Ord Edge where
    compare (E a b c) (E d e f) = if a == d && c == f && eqContent b e then EQ else LT

instance equEdge :: Eq Edge where
    eq (E a b c) (E d e f) = a == d && c == f && eqContent b e

data Content 
  = D Declaration
  | S Statement
  | B BExp 

mergeListList :: List (List Edge) -> List (List Edge) -> List (List Edge)
mergeListList (a:as) b = mergeListList as (a:b)
mergeListList Nil b = b

difference :: List Edge -> List Edge -> List Edge
difference (a:as) b = if contains a b then difference as b else (a:difference as b)
difference Nil _ = Nil

contains :: Edge -> List Edge -> Boolean
contains (E a1 b1 c1) ((E a2 b2 c2):bs) = 
  if ((a1==a2) && (eqContent b1 b2) && c1 == c2) 
  then true 
  else contains (E a1 b1 c1) bs 
contains _ Nil = false 


eqListEdge :: List Edge -> List Edge -> Boolean 
eqListEdge (E a1 b1 c1:as) (E a2 b2 c2:bs) = if ((a1==a2) && (eqContent b1 b2) && c1 == c2) 
  then eqListEdge as bs
  else false
eqListEdge Nil Nil = true
eqListEdge _ _ = false 


eqEdge :: Edge -> Edge -> Boolean
eqEdge (E a1 b1 c1) (E a2 b2 c2) = (a1==a2) && (eqContent b1 b2) && c1 == c2


eqContent :: Content -> Content -> Boolean
eqContent (D d1) (D d2) = eqDeclaration d1 d2 
eqContent (S s1) (S s2) = eqStatement s1 s2
eqContent (B b1) (B b2) = eqBExp b1 b2
eqContent _ _ = false 

eqDeclaration :: Declaration -> Declaration -> Boolean 
eqDeclaration (DVar d1) (DVar d2) = d1 == d2
eqDeclaration (DArray d1 d3) (DArray d2 d4) = d1 == d2 && d3 == d4
eqDeclaration (DRecord d1) (DRecord d2) = d1 == d2
eqDeclaration None None = true 
eqDeclaration (DDouble d1 d2) (DDouble d3 d4) = eqDeclaration d1 d3 && eqDeclaration d2 d4
eqDeclaration _ _ = false 

eqStatement :: Statement -> Statement -> Boolean
eqStatement (LDef s1 s2) (LDef s3 s4)       = eqLExp s1 s3 && eqAExp s2 s4 
eqStatement (RDef s1 s2 s3) (RDef s4 s5 s6) = s1 == s4 && eqAExp s2 s5 && eqAExp s3 s6 
eqStatement (SDouble s1 s2) (SDouble s3 s4) = eqStatement s1 s3 && eqStatement s2 s4 
eqStatement (If s1 s2) (If s3 s4)           = eqBExp s1 s3 && eqStatement s2 s4
eqStatement (Ifelse s1 s2 s3) (Ifelse s4 s5 s6) = eqBExp s1 s4 && eqStatement s2 s5 && eqStatement s3 s6
eqStatement (While s1 s2) (While s3 s4)     = eqBExp s1 s3 && eqStatement s2 s4
eqStatement (Read s1) (Read s2) = eqLExp s1 s2 
eqStatement (Write s1) (Write s2) = eqAExp s1 s2 
eqStatement _ _ = false 

eqLExp :: LExp -> LExp -> Boolean 
eqLExp (LVar a) (LVar b) = a==b
eqLExp (LArray a b) (LArray c d) = a == c && eqAExp b d
eqLExp (LRfst a) (LRfst b) = a==b
eqLExp (LRsnd a) (LRsnd b) = a==b
eqLExp _ _ = false 

eqAExp :: AExp -> AExp -> Boolean 
eqAExp (ANumber a) (ANumber b) = a==b
eqAExp (AVar a) (AVar b) = a==b
eqAExp (AArray a b) (AArray c d) = a==c && eqAExp b d
eqAExp (ARfst a) (ARfst b) = a==b
eqAExp (ARsnd a) (ARsnd b) = a==b
eqAExp (Arithmetic a b c) (Arithmetic d e f) = eqAExp a d && eqOpa b e && eqAExp c f
eqAExp _ _ = false 

eqBExp :: BExp -> BExp -> Boolean 
eqBExp True True = true
eqBExp False False = true
eqBExp (Relational a b c) (Relational d e f) = eqAExp a d && eqOpr b e && eqAExp c f
eqBExp (Boolean a b c) (Boolean d e f) = eqBExp a d && eqOpb b e && eqBExp c f
eqBExp (Negation a) (Negation b) = eqBExp a b 
eqBExp _ _ = false 

eqOpa :: Opa -> Opa -> Boolean 
eqOpa Addition Addition = true
eqOpa Substraction Substraction = true
eqOpa Multiplication Multiplication = true
eqOpa Division Division = true
eqOpa Remainder Remainder = true
eqOpa _ _ = false 

eqOpr :: Opr -> Opr -> Boolean 
eqOpr More More = true
eqOpr Less Less = true
eqOpr MoreEq MoreEq = true
eqOpr LessEq LessEq = true
eqOpr Eq Eq = true
eqOpr NotEq NotEq = true
eqOpr _ _ = false 

eqOpb :: Opb -> Opb -> Boolean 
eqOpb And And = true 
eqOpb Or Or = true 
eqOpb _ _ = false 


