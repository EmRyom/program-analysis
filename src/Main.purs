module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Parser (parse)
import Generator (generate)

type InputState = {currentText :: String}

initProof :: String
initProof = """
{
int x;
x := 10;
while (x!=0) {
  x := x-1;
}
}
"""

initState :: InputState
initState = {currentText: initProof}

showState :: InputState -> String
showState s = generate $ parse s.currentText




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
