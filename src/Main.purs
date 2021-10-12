module Main where

import Data.Either (Either(..))
import Generator (generate)
import Prelude

import AllTraversals (allTraversals, recursionLimit)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Parser (parse)
import ProgramGraph (pgGenerate)
import ReachingDefinition (rdGenerate)
import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))

type InputState = {currentText :: String}

initProof :: String
initProof = """
/* 1: Reaching Definitions 
   2: Program Graph
   3: All possible traversals (recursion depth: """ <> show recursionLimit <> """)

Choice :*/ 1

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
{-
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
-}

initState :: InputState
initState = {currentText: initProof}

showState :: InputState -> String
showState s = case parse s.currentText of 
  Right (i /\ p) -> case i of 
    1 -> rdGenerate p
    2 -> pgGenerate p
    3 -> allTraversals p 
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
