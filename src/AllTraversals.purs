module AllTraversals where 


import Data.List (List(..), (:), length, singleton, null, sortBy, uncons, unsnoc, nubBy, reverse)
import AST
import Basic
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
  Program d s -> printListList (initAllTraversals edges) <> initPG edges

findInitEdge :: List Edge -> Maybe Edge
findInitEdge (E a b c:es) = if a == 0 then Just (E a b c) else findInitEdge es
findInitEdge _ = Nothing 

initAllTraversals :: List Edge -> List (List Edge)
initAllTraversals edges = case findInitEdge edges of 
    Just e -> (reverseEach $ findAllTraversals edges (singleton e))
    Nothing -> Nil

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
  newEdges -> (forAllNewEdges edges (E a b c:avoid) newEdges)
findAllTraversals _ _ = Nil 

forAllNewEdges :: List Edge -> List Edge -> List Edge -> List (List Edge)
forAllNewEdges edges avoid (newEdge:ns) = 
    let m1 = (findAllTraversals edges (newEdge:avoid)) in 
    let m2 = (forAllNewEdges edges avoid ns) in 
    mergeListList m1 m2
forAllNewEdges _ _ _ = Nil 

findConsMDisc :: List Edge -> Int -> List Edge -> List Edge 
findConsMDisc edges i disc = 
  let candidates = findEdges edges i in 
  difference candidates disc

findEdges :: List Edge -> Int -> List Edge
findEdges (E out c inn:edges) i = 
  if out == i then (E out c inn:findEdges edges i)
  else findEdges edges i
findEdges Nil _ = Nil 