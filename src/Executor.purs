module Executor where


import AST
import Data.Either
import Data.Maybe
import Data.Tuple
import ProgramGraph

import AllTraversals (edgeIncrement, tuplefy)
import Data.List (List(..), singleton, (:), deleteBy)
import Data.Tuple.Nested ((/\))
import Prelude (show, ($), (<>), (||), (&&), (-), (==), (+), (*), (/), (/=), (<), (>), (<=), (>=), mod)


execute :: Program -> String
execute p = let edges = pgProgram p in
    genTuple (run (tuplefy edges) 0 Nil) 

recursionLimit2 :: Int 
recursionLimit2 = 100000


untuplefy :: List (Tuple Edge Int) -> List Edge
untuplefy ((a/\b):c) = (a:untuplefy c)
untuplefy Nil = Nil 



run :: List (Tuple Edge Int) -> Int -> List Memory -> Tuple (List Memory) (Maybe (Tuple  Error Int))
run edges currentNode memory = 
    let newEdges = findEdges2 edges currentNode in
    case newEdges of 
        ((E f (B a) c):((E e (B b) d):Nil)) -> case evalBExp a memory of 
            Left x -> memory /\ Just (x /\ currentNode)
            Right true   -> run (edgeIncrement edges (E f (B a) c)) c memory
            Right false  -> run (edgeIncrement edges (E e (B b) d)) d memory        
        (E b (D a) c:Nil) -> case a of 
            DVar x -> run (edgeIncrement edges (E b (D a) c)) c (memoryInitiate memory (Var x N))
            DArray x y -> run (edgeIncrement edges (E b (D a) c)) c (memoryInitiate memory (Array x (initArray y)))
            DRecord x -> run (edgeIncrement edges (E b (D a) c)) c (memoryInitiate memory (Record x N N))
            _ -> memory /\ Just (Error /\ currentNode)
        (E aa (S s) c:Nil) -> case s of 
            LDef a b -> case (evalAExp b memory) of 
                Left err -> memory /\ Just (err /\ currentNode)
                Right int -> case a of 
                    LVar x -> case (memoryReplace memory (Var x (I int))) of 
                        Right x1 -> run (edgeIncrement edges (E aa (S s) c)) c x1
                        Left err -> memory /\ Just (err /\ currentNode)
                    -- LArray c d -> run edges c (memoryReplace memory (Array c int))
                    --LRfst c
                    --LRsnd c
                    _ -> memory /\ Just (Error /\ currentNode)
            --RDef a b c -> 
            Write a -> run edges c memory
            --Write a 
            _ -> memory /\ Just (Error /\ currentNode)
        _ -> memory /\ (if highest (untuplefy edges) 0 == currentNode then Nothing else Just (RecursionLimitExceeded /\ currentNode))

--evalLExp :: LExp -> 


initArray :: Int -> List Entry 
initArray 0 = Nil
initArray i = (N:initArray (i-1))

findEdges2 :: List (Tuple Edge Int) -> Int -> List Edge
findEdges2 ((E out c inn /\ u):edges) i = 
  if out == i && u < recursionLimit2 then (E out c inn:findEdges2 edges i)
  else findEdges2 edges i
findEdges2 Nil _ = Nil 

evalBExp :: BExp -> List Memory -> Either Error Boolean
evalBExp b m = case b of
    True -> Right true
    False -> Right false
    Relational x z y -> evalRelational z (evalAExp x m) (evalAExp y m) 
    Boolean x y z -> evalBoolean y (evalBExp x m) (evalBExp z m)
    Negation x -> evalNegation (evalBExp x m)
    
evalNegation :: Either Error Boolean -> Either Error Boolean 
evalNegation (Left a) = (Left a)
evalNegation (Right a) = if (a) then Right false else Right true

evalRelational :: Opr -> Either Error Int -> Either Error Int -> Either Error Boolean
evalRelational opr (Left a) _ = Left a
evalRelational opr _ (Left a) = Left a
evalRelational opr (Right x) (Right y) = 
    case opr of 
    More -> Right (x > y)
    Less -> Right (x < y)
    MoreEq -> Right (x >= y)
    LessEq -> Right (x <= y)
    Eq -> Right (x == y)
    NotEq -> Right (x /= y)

evalBoolean :: Opb -> Either Error Boolean -> Either Error Boolean -> Either Error Boolean
evalBoolean opb (Left a) _ = Left (a)
evalBoolean opb _ (Left a) = Left (a)
evalBoolean opb (Right x) (Right y) = case opb of 
    And -> Right (x && y)
    Or -> Right (x || y)

evalAExp :: AExp -> List Memory -> Either Error Int 
evalAExp a m = case a of 
    ANumber x -> Right x
    AVar x -> (checkEntry $ varLookUp m x)
    AArray x y -> checkEntry (arrayLookUp m x (evalAExp y m))
    ARfst x -> checkEntry $ recordLookUp m x true
    ARsnd x -> checkEntry $ recordLookUp m x false
    Arithmetic x y z -> evalArithmetic y (evalAExp x m) (evalAExp z m)

evalArithmetic :: Opa -> Either Error Int -> Either Error Int -> Either Error Int
evalArithmetic opa (Left a) _ = Left a
evalArithmetic opa _ (Left a) = Left a
evalArithmetic opa (Right x) (Right y) = case opa of 
        Addition -> Right (x + y)
        Substraction -> Right (x - y)
        Multiplication -> Right (x * y)
        Division -> Right( x / y )
        Remainder -> Right (mod x y)
    


checkEntry :: Either Error Entry -> Either Error Int
checkEntry (Left a) = Left a
checkEntry (Right (I n)) = Right n
checkEntry (Right N) = Left InitError


varLookUp :: List Memory -> String ->  Either Error Entry
varLookUp (a:as) e = case a of
    Var b c -> if b == e then Right c else varLookUp as e
    _ -> varLookUp as e
varLookUp Nil _ = Left InitError

arrayLookUp :: List Memory -> String -> Either Error Int -> Either Error Entry 
arrayLookUp _ _ (Left a) = (Left a)
arrayLookUp (a:as) e (Right x) = case a of 
    Array b c -> if b == e then (index c x) else arrayLookUp as e (Right x)
    _ -> arrayLookUp as e (Right x)
arrayLookUp Nil _  x = Left Error

recordLookUp :: List Memory -> String -> Boolean -> Either Error Entry 
recordLookUp (a:as) e x = case a of 
    Record b c d -> if b == e then 
        if x 
        then Right c 
        else Right d
        else recordLookUp as e x
    _ -> recordLookUp as e x
recordLookUp Nil _ x = Left Error 


index :: List Entry -> Int -> Either Error Entry 
index (a:as) 0 = Right a
index (a:as) b = index as (b-1)
index Nil b = Left Error 




memoryInitiate :: List Memory -> Memory -> List Memory
memoryInitiate a b = (b:deleteBy eqMemory b a)

memoryReplace :: List Memory -> Memory -> Either Error (List Memory)
memoryReplace a b = Right ( mr a b )

mr :: List Memory -> Memory -> List Memory
mr a b = (b:deleteBy eqMemory b a)

eqMemory :: Memory -> Memory -> Boolean 
eqMemory (Array a _) (Array b _) = a == b
eqMemory (Var a _) (Var b _) = a == b
eqMemory (Record a _ _) (Record b _ _) = a == b 
eqMemory _ _ = false 

data Memory 
  = Array String (List Entry) 
  | Var String Entry
  | Record String Entry Entry

data Entry 
  = I Int
  | N  

data Error 
    = InitError 
    | ArrayError
    | NotInitError
    | RecursionLimitExceeded
    | Error

data Error2 
    = Err Error Edge 

return :: String
return = """
"""

genEither :: Either Error (Tuple (List Memory) Int) -> String
genEither (Left e) = genError e
genEither (Right (e /\ a)) = genMemory e <> show a

genTuple :: Tuple (List Memory) (Maybe (Tuple Error Int)) -> String
genTuple (mem /\ Just (err /\ i)) = genMemory mem <> genError err <> " at node " <> show i 
genTuple (mem /\ Nothing) = genMemory mem 


genError :: Error -> String
genError InitError = "Variable not Initialised"
genError ArrayError = "ArrayError"
genError NotInitError = "NotInitError"
genError RecursionLimitExceeded = "RecursionLimitExceeded"
genError Error = "Error"

genMemory :: List Memory -> String
genMemory ((Array a b):as) = "Array " <> a <> " = " <> genEntry b <> return <> genMemory as
genMemory ((Var a b):as) = "Var " <> a <> " = " <> genEntry (singleton b) <> return <> genMemory as
genMemory ((Record a b c):as) = "Record " <> a <> " = " <> genEntry (b:singleton c) <> return <> genMemory as 
genMemory Nil = ""


genEntry :: List Entry -> String
genEntry (I i:ins) = show i <> " " <> genEntry ins
genEntry (N:ins) = " undef " <> genEntry ins
genEntry Nil = ""
