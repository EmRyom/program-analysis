module Main where

import Prelude
import AST
import Worklist
import AllTraversals (allTraversals, recursionLimit)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import DangerousVariables (dvGenerate)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Executor (execute)
import Generator (generate)
import LiveVariables (lvGenerate)
import Parser (parse)
import ProgramGraph (pgGenerate)
import FaintVariables (fvGenerate)
import ReachingDefinition (rdGenerate)
import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))

type InputState = {currentText :: String}

initProof :: String
initProof = """/* 
  1: Program Graph
On a simple algorithm:
  2: Reaching Definitions 
  3: Dangerous Variables
  4: Live Variables
  5: Faint Variables
On the worklist algorithm:
  6: RD
Extra:
  11: All possible traversals (recursion limit: """ <> show recursionLimit <> """)
  12: Run program (variables only)
  13: Print program (back from AST)

Selection :*/ 1

{
if (x<=0) {
  while (x>0) {
    x:=(x-1);
  }
  } else {
  while (x<0) {
    x:=(x+1);
  }
  }
}"""

{-
{
int x;
int y;
int z;
read x;
read y;
z := 1;  
while (y > 0) {
    z := (z * x); 
    y := (y - 1);
    }
write z;
}

"""
int a;
int b;
read a;
read b;
while (a!=b) {
  if (a < b)  {
    b := (b - a);
  } 
  if (b < a)  {
    a := (a - b);
  }
}
write a;
"""
{
int x;
int y;
x := 1;
y := 1000000000;
while (y>=1) {
  while (x > 0) {
    x := (x+y);
  }
  while (x < 0) {
    x := (x-y);
  }
  y := (y / 10);
}
}
-}

initState :: InputState
initState = {currentText: initProof}

astP :: Program 
astP = Program (
    DDouble (DVar "x") (
    DDouble (DVar "y") None)) (
      SDouble (Read (LVar "x")) (
      SDouble (Read (LVar "y")) (
      SDouble (LDef (LVar "x") (Arithmetic (AVar "x") (Multiplication) (AVar "y")))
      (Write (AVar "x"))))) 

showState :: InputState -> String
showState s = case parse s.currentText of 
  Right (i /\ p) -> case i of 
      1 -> pgGenerate p
      2 -> rdGenerate p
      3 -> dvGenerate p 
      4 -> lvGenerate p
      5 -> fvGenerate p 
      6 -> rdWorklist p
      11 -> allTraversals p
      12 -> execute p 
      _ -> generate p

  Left e -> 
    let message = parseErrorMessage e in
    let pos = showPosition $ parseErrorPosition e in
    "Error: " <> message <> " at " <> pos

showPosition :: Position -> String
showPosition (Position pos) = "line " <> show pos.line <> " column " <> show pos.column


inputWidget :: InputState -> Widget HTML InputState
inputWidget st = D.textarea
                 [ st <$ P.onFocus
                 , ((\s -> st {currentText = s}) <<< P.unsafeTargetValue) <$> P.onChange
                 , P._id "input"
                 ] [D.text initProof]

secavWidget :: forall a. Widget HTML a
secavWidget = go initState
  where
    go s = D.div'
      [ inputWidget s
      , D.pre [P._id "output"] [ D.text $ showState s ]
               ] >>= go

main :: Effect Unit
main = runWidgetInDom "root" secavWidget
