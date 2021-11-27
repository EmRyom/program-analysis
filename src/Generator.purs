module Generator where

import AST
import Prelude (show, (<>))
import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\))
import Basic

generate :: Program -> String
generate (Program d s) = showDeclaration d <> showStatement s


printSignParsing :: SignInitialisation -> String
printSignParsing ((a /\ b):as) = a <> " = {" <> (printSigns b) <> "}" <> return <> printSignParsing as
printSignParsing Nil = ""

printSigns :: List Sign -> String
printSigns (a:as) = (case a of 
  Negative -> "-"
  Positive -> "+"
  Neutral -> "0") <> (case as of 
    Nil -> ""
    b -> "," <> printSigns as)
printSigns Nil = ""

return :: String 
return = """
"""

showStatement :: Statement -> String
showStatement x = case x of 
  LDef a b ->  showLExp a <> ":=" <> showAExp b <> ";" 
  RDef a b c ->  a <> ":=(" <> showAExp b <> "," <> showAExp c <> ");" 
  If a b -> "if (" <> showBExp a <> ") {" <>  return <> showStatement b <> return <> "}" 
  Ifelse a b c -> "if (" <> showBExp a <> ") {" <>  return <> showStatement b <> return <> "} else {" <> return <> showStatement c <> return <> "}" 
  While a b -> "while (" <> showBExp a <> ") {" <> return <> showStatement b <> return <> "}"  
  SDouble a b -> showStatement a <> return <> showStatement b 
  Read a -> "read " <> showLExp a <> ";"  
  Write a -> "write " <> showAExp a <> ";" 

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
  Negation a -> "not (" <> showBExp a <> ")"

showDeclaration :: Declaration -> String
showDeclaration x = case x of 
  DVar a -> "int " <> a <> ";"
  DArray a b ->  "int[" <> show b <> "] " <> a <> ";"
  DRecord a -> "{int fst; int snd} " <> a <> ";" 
  DDouble a b -> showDeclaration a <> return <> showDeclaration b 
  None -> return 

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
