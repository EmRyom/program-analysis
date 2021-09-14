module Generator where

import AST 

import Control.Monad.State (get, modify, put, runState)
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.List (List, (:), singleton, null, sortBy, uncons, unsnoc)
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
generate (Right p) = showProgram p

return :: String 
return ="""
"""
  
showProgram :: Program -> String 
showProgram (Program d s) = showDeclaration d <> showStatement s

showStatement :: Statement -> String
showStatement x = case x of 
  LDef a b ->  showLExp a <> ":=" <> showAExp b <> return 
  RDef a b c ->  a <> ":=" <> showAExp b <> showAExp c <> return 
  If a b -> "If: if " <> showBExp a <> " then " <> showStatement b <> return
  Ifelse a b c -> "If-else: if " <> showBExp a <> " then " <> showStatement b <> " else " <> showStatement c <> return 
  While a b -> "While: while " <> showBExp a <> showStatement b <> return 
  SDouble a b -> "S-leaf: " <> showStatement a <> return <> 
                 "S-node: " <> showStatement b <> return 
  Read a -> "Read: " <> showLExp a 
  Write a -> "Write: " <> showAExp a 

showLExp :: LExp -> String
showLExp x = case x of 
  LVar a -> "Lx: " <> a
  LArray a b -> "LArray: " <> a <> "[" <> showAExp b <> "]"
  LRfst a -> "LRfst: " <> a <> ".fst"
  LRsnd b -> "LRsnd: " <> b <> ".snd"

showAExp :: AExp -> String 
showAExp x = case x of 
  ANumber a -> "ANumber: " <> show a
  AVar a -> "AVar: " <> a 
  AArray a b -> "AArray: " <> a <> "[" <> showAExp b <> "]"
  ARfst a -> "ARfst: " <> a <> ".fst"
  ARsnd b -> "ARsnd: " <> b <> ".snd"
  Arithmetic a b c -> "ArithmeticOp: " <> showAExp a <> showOpa b <> showAExp c
  
showBExp :: BExp -> String 
showBExp x = case x of 
  True -> "true"
  False -> "false"
  Relational a b c -> "RelationalOp: " <> showAExp a <> showOpr b <> showAExp c
  Boolean a b c -> "BooleanOp: " <>showBExp a <> showOpb b <> showBExp c
  Negation a -> "Negation: not " <> showBExp a

showDeclaration :: Declaration -> String
showDeclaration x = case x of 
  DVar a -> "DVar: int " <> a <> return 
  DArray a b ->  "DArray: int[" <> show b <> "] " <> a <> return 
  DRecord a -> "DRecord: " <> a <> return 
  DDouble a b -> "D-node: " <> showDeclaration a <> return <> 
             "D-leaf: " <> showDeclaration b <> return 
  None -> "D-stop: e" <> return 

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
  a /\ b -> """digraph program_graph {rankdir=TB;
node [shape = circle]
""" <> a <> astStatement b

astDeclaration :: Int -> Declaration -> Tuple String Int 
astDeclaration i x = case x of 
  DVar a -> (makeEdge ("int " <> a) i (i+1) /\ (i+1))
  DArray a b -> (makeEdge ("int[" <> show b <> "] " <> a) i (i+1) /\ (i+1))
  None -> 


makeEdge :: String -> Int -> Int -> String
makeEdge a b c = "q" <> show b <> " -> q" <> show c <> " [label = \"" <> a """\"];
"""
  
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


