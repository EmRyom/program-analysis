module Basic where 


import Data.List (List(..), (:), head, length, singleton, null, sortBy, uncons, unsnoc, nubBy, reverse)
import AST
import ProgramGraph
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Generator
import Data.Either (Either(..))
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition)
import Data.Maybe
import Prelude (show, bind, pure, show, ($), (+), (-), (<>), (<), (==), negate, (&&))


allTraversals :: Either ParseError Program -> String
allTraversals (Left err) =
  let message = parseErrorMessage err in
  let pos = showPosition $ parseErrorPosition err in
  "Error: " <> message <> " at " <> pos
allTraversals (Right p) = let edges = pgProgram p in case p of 
  Program d s -> initAllTraversals edges <> initPG edges

findInitEdge :: List Edge -> Maybe Edge
findInitEdge (E a b c:es) = if a == 0 then Just (E a b c) else findInitEdge es
findInitEdge _ = Nothing 

initAllTraversals :: List Edge -> String
initAllTraversals edges = case findInitEdge edges of 
    Just e -> """/*
        """ <> printListList (reverseEach $ findAllTraversals edges (singleton e)) <> """*/ 
        """
    Nothing -> "All Traversals Failed"
  

reverseEach :: List (List Edge) -> List (List Edge)
reverseEach (a:as) = (reverse a:reverseEach as)
reverseEach Nil = Nil

printListList :: List (List Edge) -> String
printListList (a:as) = "T {" <> printList a <> """}
""" <> printListList as
printListList _ = ""

printList :: List Edge -> String 
printList (E a (B b) c:as) = "(" <> show a <> ", " <> showBExp b <> ", " <> show c <> """)
""" <> printList as
printList (E a (S s) c:as) = "(" <> show a <> ", " <> showStatement s <> ", " <> show c <> """)
""" <> printList as
printList (E a (D d) c:as) = "(" <> show a <> ", " <> showDeclaration d <> ", " <> show c <> """)
""" <> printList as
printList _ = ""

findAllTraversals :: List Edge -> List Edge -> List (List Edge)
findAllTraversals edges (E a b c:avoid) = 
  case findConsMDisc edges c (E a b c:avoid) of 
  Nil -> singleton (E a b c:avoid)
  newEdges -> forAllNewEdges edges (E a b c:avoid) newEdges 
findAllTraversals _ _ = Nil 

forAllNewEdges :: List Edge -> List Edge -> List Edge -> List (List Edge)
forAllNewEdges edges avoid (newEdge:ns) = 
    let m1 = (findAllTraversals edges (newEdge:avoid)) in 
    let m2 = (forAllNewEdges edges avoid ns) in 
    mergeListList m1 m2
forAllNewEdges _ _ _ = Nil 


mergeListList :: List (List Edge) -> List (List Edge) -> List (List Edge)
mergeListList (a:as) b = mergeListList as (a:b)
mergeListList Nil b = b

findConsMDisc :: List Edge -> Int -> List Edge -> List Edge 
findConsMDisc edges i disc = 
  let candidates = findEdges edges i in 
  difference candidates disc

findEdges :: List Edge -> Int -> List Edge
findEdges (E out c inn:edges) i = 
  if out == i then (E out c inn:findEdges edges i)
  else findEdges edges i
findEdges Nil _ = Nil 

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


