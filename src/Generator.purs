module Generator where

import AST

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
  LDef a b ->  showLExp a <> ":=" <> showAExp b <> ";" 
  RDef a b c ->  a <> ":=(" <> showAExp b <> "," <> showAExp c <> ");" 
  If a b -> "if (" <> showBExp a <> ") then {" <> showStatement b <> "}"
  Ifelse a b c -> "if (" <> showBExp a <> ") then {" <> showStatement b <> "} else {" <> showStatement c <> "}" 
  While a b -> "while (" <> showBExp a <> ") {" <> showStatement b <> "}" 
  SDouble a b -> showStatement a <> return <> showStatement b <> return 
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
  Negation a -> "! " <> showBExp a

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
