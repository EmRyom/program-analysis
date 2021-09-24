module ProgramGraph where 

import AST
import Generator
import Control.Monad.State (get, modify, put, runState)
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.List (List(..), (:), length, singleton, null, sortBy, uncons, unsnoc)
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
  (i /\ led) -> case pgStatement i Non s of 
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

pgStatement :: Int -> Node -> Statement -> List Edge
pgStatement ingoing (High outgoing) s = case s of 
  While a b -> let trueEdge = E 
  If a b -> 
  Ifelse a b c 
  x -> 
pgStatement ingoing (Medium outgoing) s = case s of 
  While a b
  If a b
  SDouble a (SDouble b c)
  SDouble a b
  Ifelse a b c 
  x -> 
pgStatement ingoing (Low) s = case s of 
  While a b
  If a b
  SDouble a (SDouble b c)
  SDouble a b
  Ifelse a b c 
  x -> 


data Edge = E Int String Int

data Node 
  = High Int
  | Medium  Int 
  | Low  



