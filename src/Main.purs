module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Generator (generate)
import Parser (parse)
import ProgramGraph (pgGenerate)
import ReachingDefinition (rdGenerate)
import AllTraversals (allTraversals)

type InputState = {currentText :: String}

initProof :: String
initProof = """
{
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
}
"""

initState :: InputState
initState = {currentText: initProof}

showState :: InputState -> String
showState s = rdGenerate $ parse s.currentText




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
