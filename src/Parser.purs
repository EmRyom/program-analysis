module Parser where


import AST 
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (List(..), many, (:))
import Lexer (token)
import Prelude (bind, discard, pure, ($), (*>), (<*))
import Text.Parsing.Parser (Parser, ParseError, runParser, position)
import Text.Parsing.Parser.Combinators (choice, option, optionMaybe, sepBy, try, (<?>))
import Text.Parsing.Parser.String (eof)

type SParser a = Parser String a


list2tree :: (List Statement) -> Statement
list2tree (a:Nil) = a
list2tree (a:as) = SDouble a $ list2tree as
list2tree Nil = Read $ LVar "Something went wrong"

list2tree2 :: (List Declaration) -> Declaration
list2tree2 (a:as) = DDouble a $ list2tree2 as
list2tree2 _ = None


statement :: SParser Statement
statement = fix allSta
  where 
    allSta _ = choice
      [ 
        def  
      , ifsta
      , while 
      , read 
      , write
      ] <?> "a statement"

def :: SParser Statement
def = do 
  n <- lexp 
  token.reservedOp ":="
  i <- element n
  token.reservedOp ";"
  pure $ i

element :: LExp  -> SParser Statement
element i = fix allEl 
  where 
    allEl _ = choice 
      [ 
        token.parens (do 
        v1 <- aexp 
        r <- elchoice i v1
        pure $ r)
      , (do 
        v <- aexp
        pure $ LDef i v)
      ] <?> "a statement"

elchoice :: LExp -> AExp -> SParser Statement
elchoice i v1 = case i of 
  LVar x -> fix allChoice
              where 
                allChoice _ = choice 
                  [ (do 
                    token.reservedOp ","
                    v2 <- aexp 
                    pure $ RDef x v1 v2)
                  , (do
                    pure $ LDef i v1)
                  ] <?> "a statement"
  _ -> (do 
    token.reserved " @@@ "
    pure $ LDef i v1) <?> "an arithmetic expression"

ifsta :: SParser Statement
ifsta = do 
  token.reserved "if"
  b <- token.parens bexp
  s1 <- token.braces $ statements
  a <- ifelse b s1
  pure $ a 

statements :: SParser Statement
statements = fix $ \_ -> do 
  s1 <- statement
  s2 <- many statement
  pure $ (list2tree (s1:s2))

ifelse :: BExp -> Statement -> SParser Statement
ifelse b s1 = fix allIfs 
  where 
    allIfs _ = choice 
      [ 
        (do 
        token.reserved "else"
        s2 <- token.braces $ statements
        pure $ Ifelse b (s1) (s2))
      , (do 
        pure $ If b (s1))
      ] <?> "a statement"


while :: SParser Statement 
while = do 
  token.reserved "while"
  b <- token.parens bexp
  s <- token.braces statements
  pure $ While b s


read :: SParser Statement 
read = do
  token.reserved "read"
  s <- lexp
  token.reserved ";"
  pure $ Read s 

write :: SParser Statement 
write = do
  token.reserved "write"
  s <- aexp
  token.reserved ";"
  pure $ Write s 


bexp :: SParser BExp
bexp = fix allBExp
  where 
    allBExp _ = choice 
      [ (do
        token.reserved "true"
        pure $ True)
      , (do
        token.reserved "false"
        pure $ False)
      , token.parens boolean
      , relational
      , (do 
        token.reserved "not"
        i <- bexp
        pure $ Negation i)
      ] <?> "a boolean expression"

relational :: SParser BExp
relational = do
  o1 <- aexp
  op <- opr
  o2 <- aexp
  pure $ Relational o1 op o2

boolean :: SParser BExp
boolean = fix \_ -> do
  o1 <- bexp
  op <- opb
  o2 <- bexp
  pure $ Boolean o1 op o2

opr :: SParser Opr
opr = fix allOpr 
  where 
    allOpr _ = choice 
      [ (do 
        token.reservedOp ">="
        pure $ MoreEq ) 
      , (do 
        token.reservedOp "<="
        pure $ LessEq )
      , (do 
        token.reservedOp ">"
        pure $ More ) 
      , (do 
        token.reservedOp "<"
        pure $ Less )
      , (do 
        token.reservedOp "=="
        pure $ Eq ) 
      , (do 
        token.reservedOp "!="
        pure $ NotEq )
      ] <?> "a relational operator"

opb :: SParser Opb
opb = fix allOpb 
  where 
    allOpb _ = choice 
      [ (do 
        token.reservedOp "&"
        pure $ And)
      , (do 
        token.reservedOp "|"
        pure $ Or)
      ] <?> "a relational operator"


lexp :: SParser LExp 
lexp = do
  i <- token.identifier 
  b <- lexpc i
  pure $ b
  
lexpc :: String -> SParser LExp
lexpc i = fix allLExp
  where 
    allLExp _ = choice 
      [ (do
        token.reserved ".fst"
        pure $ LRfst i)
      , (do
        token.reserved ".snd"
        pure $ LRsnd i)
      , (do 
        v <- token.brackets aexp 
        pure $ LArray i v)
      , (do 
        pure $ LVar i)] <?> "a literal expression"







aexp :: SParser AExp 
aexp = fix allAExp
  where 
    allAExp _ = choice 
      [ 
        token.parens arithmetic 
      , (do 
        i <- token.integer 
        pure $ ANumber i)
      , (do
        i <- token.identifier
        n <- aexpext i 
        pure $ n)
      ] <?> "an arithmetic expression"

aexpext :: String -> SParser AExp 
aexpext i = fix allAExpext
  where 
    allAExpext _ = choice 
      [
        (do 
        token.reserved ".fst"
        pure $ ARfst i)
      , (do 
        token.reserved ".snd"
        pure $ ARsnd i)
      , (do
        n <- token.brackets aexp 
        pure $ AArray i n)
      , (do
        pure $ AVar i)
      ] <?> "an arithmetic expression"

arithmetic :: SParser AExp 
arithmetic = fix $ \p -> do
  o1 <- aexp
  op <- opa
  o2 <- aexp
  pure $ Arithmetic o1 op o2

opa :: SParser Opa
opa = fix allOpa
  where 
    allOpa _ = choice 
      [ (do 
        token.reservedOp "+"
        pure Addition)
      , (do 
        token.reservedOp "-"
        pure Substraction)
      , (do 
        token.reservedOp "*"
        pure Multiplication)
      , (do 
        token.reservedOp "/"
        pure Division) 
      , (do 
        token.reservedOp "%"
        pure Remainder) ] <?> "an arithmetic operator"





declaration :: SParser Declaration
declaration = fix allDec 
  where 
    allDec _ = choice
      [ 
        (do 
        token.reserved "int"
        i <- dintc
        pure $ i) 
      , (do 
        token.reservedOp "{"
        token.reserved "int" 
        token.reservedOp "fst;"
        token.reserved "int" 
        token.reservedOp "snd"
        token.reserved "}"
        i <- token.identifier
        token.reservedOp ";"
        pure $ DRecord i)
      ] <?> "a declaration"

dintc :: SParser Declaration 
dintc = fix allDec2
  where 
    allDec2 _ = choice
      [  
        (do 
        i <- token.brackets $ token.integer
        n <- token.identifier
        token.reservedOp ";"
        pure $ DArray n i )
      , (do 
        i <- token.identifier
        token.reservedOp ";"
        pure $ DVar i) 
      ] <?> "a declaration"
  

program :: SParser Program
program = do
  d <- many declaration
  s <- statements
  pure $ Program (list2tree2 d) s

parse :: String -> Either ParseError Program
parse input = runParser input (token.whiteSpace *> token.braces program <* eof)

