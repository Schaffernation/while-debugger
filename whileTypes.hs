-- WHILE with Types
-- ================

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fwarn-incomplete-patterns #-}
 
module WhileTypes where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub)
import Control.Monad.Error
import Control.Monad.State
import Test.QuickCheck  

--How would we go about writing a type checker for the simple WHILE programming
--language? As we go through today's discussion, think about the following 
--questions:

--Syntax
--------

--The definition of the WHILE language, including a parser, pretty printer and
--instances of the `Arbitrary` type class are in the auxiliary file
--[WhileSyntax.lhs](WhileSyntax.html) that can be downloaded
--[here](WhileSyntax.lhs). 

import WhileSyntax 

--Semantics of WHILE programming language           
-----------------------------------------  

--This semantics is the similar to that found in Homeworks [3](http://www.seas.upenn.edu/~cis552/13fa/hw/hw03/index.html) and [5](http://www.seas.upenn.edu/~cis552/hw/hw05/)

--As before, we'll use the State monad to keep track of the values of variables.

--data Store = Store (Map Variable Value) deriving (Eq, Show)

--And an error monad to enable runtime errors.

--type EvMonad a = StateT Store (Either String) a

--We'll package these up as an *Evaluation Monad*

-- Empty store
empty :: Store              
empty = Store Map.empty
         
-- lookup variables in the store
slookup :: Variable -> EvMonad Value
slookup x = do 
  Store m <- get
  case (Map.lookup x m) of
    Just v  -> return v
    Nothing -> throwError "Uninitialized variable"
 
-- update the value of a variable in the store
update :: Variable -> Value -> EvMonad ()
update x v = do 
   (Store m) <- get 
   put (Store (Map.insert x v m))

evalE :: Expression -> EvMonad Value
evalE (Var x)      = slookup x
evalE (Val v)      = return v
evalE (Op o e1 e2) = do 
   v1 <- evalE e1  
   v2 <- evalE e2           
   evalB o v1 v2

evalB :: Bop -> Value -> Value -> EvMonad Value
evalB Plus   (IntVal i1) (IntVal i2) = return $ IntVal (i1 + i2)
evalB Minus  (IntVal i1) (IntVal i2) = return $ IntVal (i1 - i2)
evalB Times  (IntVal i1) (IntVal i2) = return $ IntVal (i1 * i2)
evalB Gt     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 > i2)
evalB Ge     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 >= i2)
evalB Lt     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 < i2)
evalB Le     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 <= i2)
evalB _ _ _ = throwError "Incorrect arguments to boolean operator"

evalS :: Statement -> EvMonad ()
evalS w@(While e s)    = do 
  v <- evalE e
  case v of    
    BoolVal True  -> evalS (Sequence s w)
    BoolVal False -> return ()
    _             -> throwError "While expects boolean expression"
evalS Skip             = return ()
evalS (Sequence s1 s2) = evalS s1 >> evalS s2
evalS (Assign x e)     = do 
    v <- evalE e
    update x v 
evalS (If e s1 s2)      = do
    v <- evalE e 
    case v of 
      BoolVal True  -> evalS s1
      BoolVal False -> evalS s2
      _ -> throwError "If expects boolean expression"


--The top-level loop
--------------------

repl :: IO ()
repl = repl' empty where
  repl' store = do
    putStr "%> "
    line <- getLine
    case parseCmd line of 
      Statement stmt -> 
          case (execStateT (evalS stmt) store) of
            Left err -> putStrLn err >> repl' store
            Right store' -> repl' store'
      Expression exp -> do
          case (evalStateT (evalE exp) store) of 
            Left err -> putStrLn err 
            Right v  -> putStrLn $ display v
          repl' store
      _ -> putStrLn "what?!?" >> repl' store 


--Why types?
------------

--What is bad about this language? 

--For one thing, it's hard to reason about programs written in this
--language. Recall these properties from the quickcheck lecture. They seemed
--very reasonable, yet sadly they are false.

instance Arbitrary Store where
   arbitrary        = liftM (Store . Map.fromList) arbitrary
   shrink (Store m) = map (Store . Map.fromList) (shrink (Map.toList m))


(===) ::  Expression -> Expression -> Property
e1 === e2 = forAll arbitrary $ \st -> evalStateT (evalE e1) st == evalStateT (evalE e2) st

prop_add_zero_elim :: Property
prop_add_zero_elim = forAll arbitrary $ \ e ->
 (Op Plus e $ Val (IntVal 0)) === e 

prop_sub_zero_elim :: Property
prop_sub_zero_elim = forAll arbitrary $ \ e -> 
  (Op Minus e $ Val (IntVal 0)) === e

--Type-checking WHILE
---------------------

--What if we wanted to create a version of the WHILE language that does
--type checking at compile time? 

--We can view type checking as an *abstraction* of the evaluator. I.e.  instead
--of calculating a precise value for expressions, we'll approximate those values
--with types. 

--We have two sorts of values in this language:

--    data Value =  IntVal Int | BoolVal Bool deriving (Eq, Show)

--So we'll need exactly two types of values:

data Type =
    IntTy  
  | BoolTy
    deriving (Eq, Show)              

--Wherever we use `Value` in the evaluator, we'll replace that 
--usage with a `Type` instead. 

--For example, we'll define a monad for the type checker, and then 
--analogous functions for `evalE`, `evalB`, and `evalS`.

   typeE :: Expression -> TcMonad Type
   typeB :: Bop -> Type -> Type -> TcMonad Type
   typeS :: Statement -> TcMonad ()

--However, we'll diverge a bit in the treatment of variables. Like most
--programming languages, we'll expect the types of variables to be invariant
--throughout the execution. 

--That means we'll reject some perfectly fine programs, such as:

--   X := true;
--   X := 1

--Instead, we'll brutally require that variables have a single type.  

--But how do we determine that type? The WHILE language doesn't include variable
--declarations! 

--For simplicity, we'll use an old hack. If the name of the variable starts with
--a letter at the beginning of the alphabet (like 'A', 'B', etc.) we'll assume
--that it is a boolean variable.  If it starts with a letter at the end, we'll
--decree that it must be an integer variable.

typeVar :: Variable -> TcMonad Type
typeVar (V x) = case x of 
   (h : _) | h >= 'A' && h <= 'L' -> return BoolTy
   (h : _) | h >= 'M' && h <= 'Z' -> return IntTy
   _ -> throwError "invalid variable name"

--That means that our type checking monad only needs to keep track of typing
--errors, it doesn't need to remember any information about the types of
--variables.

type TcMonad a = Either String a

typeE :: Expression -> TcMonad Type
typeE = undefined

typeB :: Bop -> Type -> Type -> TcMonad Type
typeB = undefined 

typeS :: Statement -> TcMonad ()
typeS = undefined


--Why do variables have types?
-----------------------------

--Why do we need to require that variables have constant types throughout their
--execution?  It's tempting to define the type monad by analogy with the
--expression monad---if we did so, we could track the types of variables at any
--point in the program. Thus,

--     type TcMonad a = StateT (Map Variable Type) (Either String) a

--However, this doesn't work. Should the following WHILE program 
--type check?

--     if false 
--        then X := false 
--        else X := 3 
--     endif;
--     Y := X + 4

--We know that this program will produce the value 7, but the type checker can't
--know that. Recall that while type checking `if` statements, the type checker
--only knows that the type of the expression is a boolean. It doesn't know
--whether that boolean will be true or false, it's computing an approximate
--value. That means that the type checker has to check both branches of an `if`
--statement, in sequence. But in doing so, we would be passing the type store
--from one branch to another. That doesn't make sense!

--The REVISED top-level loop
----------------------------

--Now we can revise our top-level loop to do type checking FIRST, before
--evaluation.

tyrepl :: IO ()
tyrepl = repl' empty where
  repl' store = do
    putStr "%> "
    line <- getLine
    case parseCmd line of 
      Statement stmt -> 
          case typeS stmt of 
            Left err -> putStrLn ("TypeError: " ++ err) >> repl' store
            Right () ->
              case (execStateT (evalS stmt) store) of
                Left err -> putStrLn err >> repl' store
                Right store' -> repl' store'
      Expression exp -> 
          case typeE exp of 
             Left err -> putStrLn ("TypeError: " ++ err) >> repl' store
             Right _ -> do
                case (evalStateT (evalE exp) store) of 
                   Left err -> putStrLn err 
                   Right v  -> putStrLn $ display v
                repl' store
      _ -> putStrLn "what?!?" >> repl' store 


--Type Soundness
----------------

--How do we know that our type system is right? If a program type checks, what
--does that mean?

--A strong static type system is one that has the type soundness property.

--*Type Soundness: If a program `p` type checks, then evaluating `p` will not
--produce a runtime error.*

--For now, we can think about trying to test this property.

prop_no_errorE :: Expression -> Property
prop_no_errorE e = forAll arbitrary $ \s -> typeChecks e ==>
  case runStateT (evalE e) s of 
     Left err -> False
     Right v  -> True

typeChecks :: Expression -> Bool
typeChecks e = case typeE e of 
   Left err -> False
   Right ty -> True

--Unfortunately, we can't test the analogous property for statements. Why not?


