module AllTraversals where 


import AST (Program(..))
import Basic (eqEdge, mergeListList)
import Data.Maybe (Maybe(..))
import Data.Ordering (Ordering(..))
import Generator (showBExp, showDeclaration, showStatement)
import ProgramGraph (Content(..), Edge(..), pgProgram)
import Data.List (List(..), length, reverse, singleton, sortBy, (:))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude (Ordering, show, ($), (&&), (+), (<), (<>), (==))

allTraversals :: Program -> String
allTraversals p = let edges = pgProgram p in case p of 
  Program d s -> printListList (sortBy l (initAllTraversals edges))

recursionLimit :: Int
recursionLimit = 2

l :: List Edge -> List Edge -> Ordering
l a b = if length a == length b then EQ else 
  if length a < length b then LT else GT

findInitEdge :: List Edge -> Maybe Edge 
findInitEdge (E a b c:es) = if a == 0 then Just (E a b c) else findInitEdge es
findInitEdge _ = Nothing 

initAllTraversals :: List Edge -> List (List Edge)
initAllTraversals edges = case findInitEdge edges of 
    Just (e) -> (findAllTraversals (edgeIncrement (tuplefy edges) e) (singleton e))
    Nothing -> Nil

tuplefy :: List Edge -> List (Tuple Edge Int) 
tuplefy (a:as) = ((a /\ 0):tuplefy as)
tuplefy Nil = Nil 

printListList :: List (List Edge) -> String
printListList (a:as) = "Length:" <> (show $ length (a)) <> """
""" <> printList a <> """
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

findAllTraversals :: List (Tuple Edge Int) -> List Edge -> List (List Edge)
findAllTraversals edges (E a b c:avoid) = 
  case findEdges edges c of 
  Nil -> singleton $ reverse (E a b c:avoid)
  newEdges -> (forAllNewEdges edges (E a b c:avoid) newEdges)
findAllTraversals _ _ = Nil 

forAllNewEdges :: List (Tuple Edge Int) -> List Edge -> List Edge -> List (List Edge)
forAllNewEdges edges avoid (newEdge:ns) = 
    let m1 = (findAllTraversals (edgeIncrement edges newEdge) (newEdge:avoid)) in 
    let m2 = (forAllNewEdges edges avoid ns) in 
    mergeListList m1 m2
forAllNewEdges _ _ _ = Nil 

edgeIncrement :: List (Tuple Edge Int) -> Edge -> List (Tuple Edge Int)
edgeIncrement ((a /\ b):as) c = if eqEdge a c then ((a /\ (b+1)):as) else ((a /\ b):edgeIncrement as c)
edgeIncrement Nil c = Nil

findEdges :: List (Tuple Edge Int) -> Int -> List Edge
findEdges ((E out c inn /\ u):edges) i = 
  if out == i && u < recursionLimit then (E out c inn:findEdges edges i)
  else findEdges edges i
findEdges Nil _ = Nil 