module ProgramGraph where 

import AST (BExp(..), Declaration(..), Program(..), Statement(..))
import Generator (return, showBExp, showDeclaration, showStatement)
import Data.List (List(..), singleton, (:))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude (show, ($), (+), (<>), (<), (==),(||))

pgGenerate :: Program -> String
pgGenerate p = initPG $ pgProgram p

edgesConcat :: (List Edge) -> (List Edge) -> (List Edge)
edgesConcat (Nil) b = b
edgesConcat (a:as) (b) = edgesConcat (as) (a:b)

initPG :: (List Edge) -> String 
initPG a = """/* Paste this into dreampuf.github.io/GraphvizOnline */

digraph program_graph {rankdir=TB;
node [shape = circle]
""" <> toPG a <> """}
"""

toPG :: (List Edge) -> String 
toPG (E b t d:as) = case t of 
  D x -> makeEdge (showDeclaration x) b d <> toPG as
  S x -> makeEdge (showStatement x) b d <> toPG as
  B x -> makeEdge (showBExp x) b d <> toPG as
toPG _ = ""

makeEdge :: String -> Int -> Int -> String
makeEdge a b c = "q" <> show b <> " -> q" <> show c <> " [label = \"" <> a <> "\"];" <> return 


pgProgram :: Program -> (List Edge)
pgProgram (Program d s) = case pgDeclaration 0 d of 
  (i /\ led) -> case pgStatement i s of 
    (f /\ les) -> edgesConcat led les

pgDeclaration :: Int -> Declaration -> Tuple Int (List Edge)
pgDeclaration l d = case d of 
  None -> (l /\ Nil)
  DDouble x1 x2 -> case pgDeclaration l x1 of 
    (i /\ led) -> case pgDeclaration i x2 of 
      (f /\ led2) -> (f /\ edgesConcat led led2)
  _ -> ((l+1) /\ singleton (E l (D d) (l+1)))

pgStatement :: Int -> Statement -> Tuple Int (List Edge)
pgStatement lastNode s = case s of
  SDouble a b -> case pgStatement lastNode a of 
    (i /\ les) -> case pgStatement i b of 
      (f /\ les2) -> f /\ edgesConcat les les2
  If a b -> let trueEdge = E lastNode (B a) (lastNode+1) in 
    case pgStatement (lastNode+1) b of 
      (i /\ les) -> let falseEdge = E lastNode (B $ Negation a) i in 
        (i /\ (edgesConcat (trueEdge:les) (singleton falseEdge)))
  Ifelse a b1 b2 -> let trueEdge = E lastNode (B a) (lastNode+1) in 
    case pgStatement (lastNode+1) b1 of 
      (i /\ les) -> let falseEdge = E lastNode (B $ Negation a) (i) in
        case pgStatement (i) b2 of 
          (f /\ les2) -> let redded = redirect les i f in 
            f /\ (edgesConcat (trueEdge:redded) (falseEdge:les2))
  While a b -> let trueEdge = E lastNode (B a) (lastNode+1) in 
    case pgStatement (lastNode+1) b of 
      (i /\ (les)) -> let redded = redirect les i lastNode in 
                      let newNode = (highest redded 0) + 1 in 
                      let falseEdge = E lastNode (B $ Negation a) newNode in 
        newNode /\ (edgesConcat (trueEdge:redded) (singleton falseEdge))    
  _ -> (lastNode+1) /\ singleton (E lastNode (S s) (lastNode+1))

redirect :: List Edge -> Int -> Int -> List Edge
redirect ((E i s o):tail) n m = 
  if o==n 
  then ((E i s m):(redirect tail n m))
  else ((E i s o):(redirect tail n m))
redirect Nil n m = Nil 

highest :: List Edge -> Int -> Int
highest ((E h _ g):as) b = if b < g || b < h 
  then if g < h then highest as h else highest as g
  else highest as b
highest Nil b = b

data Edge = E Int Content Int
 
data Content 
  = D Declaration
  | S Statement
  | B BExp 


