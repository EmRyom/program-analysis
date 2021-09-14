module AST where 

data Program = Program (Declaration) (Statement)

data Statement 
  = LDef LExp AExp
  | RDef String AExp AExp 
  | SDouble Statement Statement
  | If BExp Statement
  | Ifelse BExp Statement Statement
  | While BExp Statement 
  | Read LExp
  | Write AExp

data LExp  
  = LVar String   
  | LArray String AExp
  | LRfst String
  | LRsnd String

data AExp
  = ANumber Int 
  | AVar String
  | AArray String AExp
  | ARfst String
  | ARsnd String
  | Arithmetic AExp Opa AExp

data BExp 
  = True
  | False
  | Relational AExp Opr AExp
  | Boolean BExp Opb BExp
  | Negation BExp

data Declaration 
  = DVar String
  | DArray String Int
  | DRecord String
  | None
  | DDouble Declaration Declaration

data Opa  
  = Addition 
  | Substraction
  | Multiplication
  | Division
  | Remainder

data Opr 
  = More
  | Less
  | MoreEq
  | LessEq
  | Eq
  | NotEq

data Opb
  = And 
  | Or
