module Generator where

import AST 

import Control.Monad.State (get, modify, put, runState)
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.List (List(..), (:), singleton, null, sortBy, uncons, unsnoc)
import Data.Map (Map, empty, insert, isEmpty, lookup, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Ord (compare)
import Data.String.CodeUnits (drop, dropRight)
import Data.String.Utils (unsafeRepeat)
import Data.Traversable (intercalate, traverse)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Prelude (show, bind, pure, show, ($), (+), (-), (<>), (<))
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))

showPosition :: Position -> String
showPosition (Position pos) = "line " <> show pos.line <> " column " <> show pos.column

generate :: Either ParseError Program -> String
generate (Left err) =
  let message = parseErrorMessage err in
  let pos = showPosition $ parseErrorPosition err in
  "Error: " <> message <> " at " <> pos
generate (Right p) = initPG $ pgProgram p -- showProgram p

return :: String 
return ="""
"""
  
showProgram :: Program -> String 
showProgram (Program d s) = showDeclaration d <> showStatement s

showStatement :: Statement -> String
showStatement x = case x of 
  LDef a b ->  showLExp a <> ":=" <> showAExp b <> ";" 
  RDef a b c ->  a <> ":=(" <> showAExp b <> "," <> showAExp c <> ");" 
  If a b -> "if (" <> showBExp a <> ") then {" <> showStatement b <> "}"
  Ifelse a b c -> "if (" <> showBExp a <> ") then {" <> showStatement b <> "} else {" <> showStatement c <> "}" 
  While a b -> "while (" <> showBExp a <> ") {" <> showStatement b <> "}" 
  SDouble a b -> showStatement a <> return <> showStatement b <> return 
  Read a -> "read " <> showLExp a 
  Write a -> "write " <> showAExp a 

showLExp :: LExp -> String
showLExp x = case x of 
  LVar a -> a
  LArray a b -> a <> "[" <> showAExp b <> "]"
  LRfst a -> a <> ".fst"
  LRsnd b -> b <> ".snd"

showAExp :: AExp -> String 
showAExp x = case x of 
  ANumber a -> show a
  AVar a -> a 
  AArray a b -> a <> "[" <> showAExp b <> "]"
  ARfst a -> a <> ".fst"
  ARsnd b -> b <> ".snd"
  Arithmetic a b c -> showAExp a <> showOpa b <> showAExp c
  
showBExp :: BExp -> String 
showBExp x = case x of 
  True -> "true"
  False -> "false"
  Relational a b c -> showAExp a <> showOpr b <> showAExp c
  Boolean a b c -> showBExp a <> showOpb b <> showBExp c
  Negation a -> "not " <> showBExp a

showDeclaration :: Declaration -> String
showDeclaration x = case x of 
  DVar a -> "int " <> a <> ";"
  DArray a b ->  "int[" <> show b <> "] " <> a <> ";"
  DRecord a -> "{int fst; int snd} " <> a <> ";" 
  DDouble a b -> showDeclaration a <> return <> showDeclaration b <> return 
  None -> "e" 

showOpa :: Opa -> String 
showOpa x = case x of 
  Addition -> " + "
  Substraction -> " - "
  Multiplication -> " * "
  Division -> " / "
  Remainder -> " % "

showOpr :: Opr -> String
showOpr x = case x of 
  More -> " > "
  Less -> " < "
  MoreEq -> " >= "
  LessEq -> " <= "
  Eq -> " == "
  NotEq -> " != "

showOpb :: Opb -> String 
showOpb x = case x of 
  And -> " & "
  Or -> " | "

{-
astGraph :: Int -> Program -> String 
astGraph i (Program a b) = case astDeclaration i a of 
  c /\ d -> """digraph program_graph {rankdir=TB;
node [shape = circle]
""" <> c <> astStatement d b

astDeclaration :: Int -> Declaration -> Tuple String Int 
astDeclaration i x = case x of 
  DVar a -> (makeEdge ("int " <> a) i (i+1) /\ (i+1))
  DArray a b -> (makeEdge ("int[" <> show b <> "] " <> a) i (i+1) /\ (i+1))
  None -> (makeEdge "e" i (i+1)) /\ (i+1)
  DRecord a -> (makeEdge ("record " <> a) i (i+1) ) /\ (i+1)
  DDouble a b -> case astDeclaration i a of 
    (c /\ d) -> case astDeclaration d b of
      (e /\ f) -> ((c <> e) /\ f)

astStatement :: Int -> Statement -> String 
astStatement _ _ = "}" <> return 


makeEdge :: String -> Int -> Int -> String
makeEdge a b c = "q" <> show b <> " -- q" <> show c <> " [label = \"" <> a <> "\"];" <> return 



digraph program_graph {rankdir=TB;
node [shape = circle]; q_start;
node [shape = doublecircle]; q_end; 
node [shape = circle]
q_start -> q1 [label = "i:=1"];
q1 -> q2 [label = "i<n"];
q2 -> q3 [label = "j:=i"];
q3 -> q5 [label = "j>0"];
q5 -> q6 [label = "A[j-1]>A[j]"];
q6 -> q7 [label = "t:=A[j]"];
q7 -> q8 [label = "A[j]:=A[j-1]"];
q8 -> q9 [label = "A[j-1]:=t"];
q9 -> q3 [label = "j:=j-1"];
q5 -> q10 [label = "A[j-1]<=A[j]"];
q10 -> q3 [label = "j:=0"];
q3 -> q4 [label = "!(j>0)"];
q4 -> q1 [label = "i:=i+1"];
q1 -> q_end [label = "!(i<n)"];
}
-}

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
pgStatement l s = case s of
  LDef a b -> (l+1) /\ singleton(E l (showLExp a <> ":=" <> showAExp b) (l+1))
  RDef a b1 b2 -> (l+1) /\ singleton (E l (a <> ":=(" <> showAExp b1 <> "," <> showAExp b2 <> ")") (l+1))
  SDouble a b -> case pgStatement l a of 
    (i /\ les) -> case pgStatement i b of 
      (f /\ les2) -> f /\ edgesConcat les les2
  If a b -> let trueEdge = E l (showBExp a) (l+1) in 
    case pgStatement (l+1) b of 
      (i /\ les) -> (i /\ (edgesConcat (trueEdge:les) (singleton (E l ("!" <> showBExp a) i))))
  Ifelse a b1 b2 -> let trueEdge = E l (showBExp a) (l+1) in 
    case pgStatement (l+1) b1 of 
      (i /\ les) -> let falseEdge = E l ("!" <> showBExp a) (i+1) in
        case pgStatement (i+1) b2 of 
          (f /\ les2) -> let connecting = E i "" f in
            (f /\ edgesConcat (trueEdge:les) (edgesConcat (falseEdge:les2) (singleton connecting)))
  While a b -> let trueEdge = E l (showBExp a) (l+1) in 
    case pgStatement (l+1) b of 
      (i /\ les) -> (i /\ (edgesConcat (trueEdge:les) (singleton (E i "" l))))
  Read a -> (l+1) /\ singleton (E l ("read " <> showLExp a) (l+1))
  Write a -> ((l+1) /\ singleton (E l ("write " <> showAExp a) (l+1)))









