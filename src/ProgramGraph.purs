module ProgramGraph where 

import AST
import Generator
import Control.Monad.State (get, modify, put, runState)
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.List (List(..), (:), length, singleton, null, sortBy, uncons, unsnoc, reverse)
import Data.Map (Map, empty, insert, isEmpty, lookup, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Ord (compare)
import Data.String.CodeUnits (drop, dropRight)
import Data.String.Utils (unsafeRepeat)
import Data.Traversable (intercalate, traverse)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Prelude (show, bind, pure, show, ($), (+), (-), (<>), (<), (==))
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))

pgGenerate :: Either ParseError Program -> String
pgGenerate (Left err) =
  let message = parseErrorMessage err in
  let pos = showPosition $ parseErrorPosition err in
  "Error: " <> message <> " at " <> pos
pgGenerate (Right p) = initPG $ pgProgram p

edgesConcat :: (List Edge) -> (List Edge) -> (List Edge)
edgesConcat (Nil) b = b
edgesConcat (a:as) (b) = edgesConcat (as) (a:b)

initPG :: (List Edge) -> String 
initPG a = """digraph program_graph {rankdir=TB;
node [shape = circle]
""" <> toPG a <> """
}
"""

toPG :: (List Edge) -> String 
toPG (E b t d:as) = makeEdge t b d <> toPG as
toPG _ = ""

makeEdge :: String -> Int -> Int -> String
makeEdge a b c = "q" <> show b <> " -> q" <> show c <> " [label = \"" <> a <> "\"];" <> return 


pgProgram :: Program -> (List Edge)
pgProgram (Program d s) = case pgDeclaration 0 d of 
  (i /\ led) -> case pgStatement i s of 
    (f /\ les) -> edgesConcat led les

pgDeclaration :: Int -> Declaration -> Tuple Int (List Edge)
pgDeclaration l d = case d of 
  DVar x -> ((l+1) /\ singleton (E l ("int " <> x) (l+1)))
  DArray x t -> (l+1) /\ singleton (E l ("int[" <> show t <> "]" <> x) (l+1))
  DRecord x -> (l+1) /\ singleton (E l ("record " <> x) (l+1))
  None -> (l /\ Nil)
  DDouble x1 x2 -> case pgDeclaration l x1 of 
    (i /\ led) -> case pgDeclaration i x2 of 
      (f /\ led2) -> (f /\ edgesConcat led led2)

pgStatement :: Int -> Statement -> Tuple Int (List Edge)
pgStatement lastNode s = case s of
  SDouble a b -> case pgStatement lastNode a of 
    (i /\ les) -> case pgStatement i b of 
      (f /\ les2) -> f /\ edgesConcat les les2
  If a b -> let trueEdge = E lastNode (showBExp a) (lastNode+1) in 
    case pgStatement (lastNode+1) b of 
      (i /\ les) -> let falseEdge = E lastNode ("!" <> showBExp a) i in 
        (i /\ (edgesConcat (trueEdge:les) (singleton falseEdge)))
  Ifelse a b1 b2 -> let trueEdge = E lastNode (showBExp a) (lastNode+1) in 
    case pgStatement (lastNode+1) b1 of 
      (i /\ les) -> let falseEdge = E lastNode ("!" <> showBExp a) (i+1) in
        case pgStatement (i+1) b2 of 
          (f /\ les2) -> let redded = redirect les i f in 
            f /\ (edgesConcat (trueEdge:redded) (falseEdge:les2))
  While a b -> let trueEdge = E lastNode (showBExp a) (lastNode+1) in 
    case pgStatement (lastNode+1) b of 
      (i /\ (les)) -> let redded = redirect les i lastNode in 
                      let newNode = (highest redded 0) + 2 in 
                      let falseEdge = E lastNode ("!" <> showBExp a) newNode in 
        newNode /\ (edgesConcat (trueEdge:redded) (singleton falseEdge))    
  _ -> (lastNode+1) /\ singleton (E lastNode (showStatement s) (lastNode+1))

redirect :: List Edge -> Int -> Int -> List Edge
redirect ((E i s o):tail) n m = 
  if o==n 
  then ((E i s m):(redirect tail n m))
  else ((E i s o):(redirect tail n m))
redirect Nil n m = Nil 

highest :: List Edge -> Int -> Int
highest ((E _ _ g):as) b = if b < g then highest as g else highest as b
highest Nil b = b

data Edge = E Int String Int
 



