{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
FlexibleInstances #-}

module WhileStep where

import WhilePP
import WhileParser

import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Test.HUnit hiding (State)


type VarMap  = Map Variable Value
type Store   = VarMap

emptyStore :: Store
--emptyStore = (Map.empty, Nothing, [])
emptyStore = Map.empty

getLn :: Statement -> Int
getLn (Assign _ _ ln) = ln
getLn (If _ _ _ ln)   = ln
getLn (While _ _ ln)  = ln
getLn (Sequence st _) = getLn st
getLn (Skip ln)       = ln
getLn (Print _ _ ln)  = ln

atLn :: Statement -> String
atLn s = " at line " ++ show (getLn s)

makeNext :: Statement -> Maybe Statement -> Maybe Statement
makeNext s Nothing   = Just s
makeNext s (Just s') = Just $ Sequence s s'

evalEError :: (MonadState Store m, MonadError String m, MonadWriter String m) => 
         Statement -> Expression -> m Value
evalEError s e = catchError (evalE e) (\str -> throwError (str ++ atLn s))

evalS :: (MonadState Store m, MonadError String m, MonadWriter String m) => Statement -> m (Maybe Statement)
evalS stmt = 
  case stmt of
    Assign var expr _ -> do
      dict <- get
      val  <- evalEError stmt expr
      put (Map.insert var val dict)
      return Nothing
    If expr s1 s2 _ -> do
      val <- evalEError stmt expr
      case val of
        BoolVal True  -> return $ Just s1
        BoolVal False -> return $ Just s2
        IntVal  _     -> throwError $ "Expected Bool in if guard" ++ atLn stmt
    While expr s _  -> do
      val <- evalEError stmt expr
      case val of
        BoolVal True  -> return $ Just $ Sequence s stmt
        BoolVal False -> return Nothing
        IntVal  _     -> throwError $ "Expected Bool in while guard" ++ atLn stmt
    Sequence s1 s2 -> do
      ms <- evalS s1
      case ms of
        Nothing -> return $ Just s2
        Just s  -> return $ Just $ Sequence s s2
    Skip _            -> return Nothing
    Print str expr  _ -> do
      val <- evalEError stmt expr
      tell $ str ++ (show $ pp val) ++ "\n"
      return Nothing

-- EvalE -------------------------------------------

evalBinop :: (MonadState Store m, MonadError String m, MonadWriter String m) => 
             (a -> Value) -> (Int -> Int -> a) -> Expression -> Expression -> 
             m Value
evalBinop v f exp1 exp2 = 
  do
    e1 <- (evalE exp1)
    e2 <- (evalE exp2)
    case (e1, e2) of
      (IntVal x, IntVal y) -> return $ v (f x y)
      (_, _) -> throwError "Binary Operation expected integers"

evalBop :: (MonadState Store m, MonadError String m, MonadWriter String m) => 
           Bop -> Expression -> Expression -> m Value
evalBop Plus   = evalBinop (IntVal) (+)
evalBop Minus  = evalBinop (IntVal) (-)
evalBop Times  = evalBinop (IntVal) (*)
evalBop Divide = \e1 e2 -> do 
  v1 <- (evalE e1)
  v2 <- (evalE e2)
  case (v1, v2) of
      (IntVal x, IntVal y) -> 
        if y == 0 then 
          throwError "Divide by zero? nooo.."
        else
          return $ IntVal (div x y)
      (_, _) -> throwError "Binary Operation expected integers"
evalBop Gt     = evalBinop (BoolVal) (>)
evalBop Ge     = evalBinop (BoolVal) (>=)
evalBop Lt     = evalBinop (BoolVal) (<)
evalBop Le     = evalBinop (BoolVal) (<=)

evalE :: (MonadState Store m, MonadError String m, MonadWriter String m) => 
         Expression -> m Value
evalE (Var var)    = 
  do
    dict <- get
    case Map.lookup var dict of
      Just val -> return val
      Nothing  -> throwError "Variable not in scope"
evalE (Val v)    = return v
evalE (Op bop exp1 exp2) = evalBop bop exp1 exp2

----------------------------------------------------

type MyMonad = (ErrorT String (WriterT String (State Store)))

complete :: (MonadState Store m, MonadError String m, MonadWriter String m) => Statement -> m (Maybe Statement)
complete stmt = do
  next <- evalS stmt
  case next of
    Just stmt' -> complete stmt'
    Nothing    -> return Nothing

breakOn :: (MonadState Store m, MonadError String m, MonadWriter String m) => [Int] -> Statement -> m (Maybe Statement)
breakOn bps stmt = do
  next <- evalS stmt
  case next of
    Nothing    -> return Nothing
    Just stmt' ->
      if getLn stmt' `elem` bps then return (Just stmt') else breakOn bps stmt'

execute :: (Statement -> MyMonad (Maybe Statement)) -> Store -> Statement -> (Store, Maybe String, String, Maybe Statement)
execute func st stmt = (st', posErr, printLog, next)
  where 
    result :: MyMonad (Maybe Statement)
    result = func stmt
    postError  :: WriterT String (State Store) (Either String (Maybe Statement))
    postError  =  runErrorT result
    postWriter :: State Store ((Either String (Maybe Statement)), String)
    postWriter =  runWriterT postError
    postState  :: (((Either String (Maybe Statement)), String), Store)
    postState  =  runState postWriter st
    ((err, printLog), st') = postState
    (posErr, next) = case err of
      Right s -> (Nothing, s)
      Left  v -> (Just v, Nothing) 

step :: Store -> Statement -> (Store, Maybe String, String, Maybe Statement)
step = execute evalS


run :: [Int] -> Store -> Statement -> (Store, Maybe String, String, Maybe Statement)
run bps = execute (breakOn bps)

stepAgain :: (Store, Maybe String, String, Maybe Statement) -> (Store, Maybe String, String, Maybe Statement)
stepAgain (dict,_,_, Just next) = step dict next
stepAgain t@(_,_,_,Nothing) = t


raises :: Statement -> String -> Test
s `raises` str = case (run [] emptyStore s) of
    (_, Just str', _, _) -> str ~?= str'
    _  -> undefined

--t1 :: Test
--t1 = (Assign "X"  (Var "Y")) `raises` IntVal 0

--t2 :: Test
--t2 = (Assign "X" (Op Divide (Val (IntVal 1)) (Val (IntVal 0)))) `raises` IntVal 1

--t3 :: Test       
--t3 = TestList [ Assign "X" (Op Plus (Val (IntVal 1)) (Val (BoolVal True))) `raises` IntVal 2,      
--                If (Val (IntVal 1)) Skip Skip `raises` IntVal 2,
--                While (Val (IntVal 1)) Skip `raises` IntVal 2]

mksequence :: [Statement] -> Statement
mksequence = foldr1 Sequence

--testprog1 :: Statement
--testprog1 = mksequence [Assign "X" $ Val $ IntVal 0,
--                        Assign "Y" $ Val $ IntVal 1,
--                        Print "hello world: " $ Var "X",
--                        If (Op Lt (Var "X") (Var "Y")) (Throw (Op Plus (Var "X") (Var "Y")))
--                                                       Skip,
--                        Assign "Z" $ Val $ IntVal 3]

----t4 :: Test
----t4 = run emptyStore testprog1 ~?=
----  ((Map.fromList [("X", IntVal 0), ("Y",  IntVal 1)]), Just (IntVal 1), "hello world: 0", Nothing)

--testprog2 :: Statement
--testprog2 = mksequence [Assign "X" $ Val $ IntVal 0,
--                        Assign "Y" $ Val $ IntVal 1,
--                        Try (If (Op Lt (Var "X") (Var "Y"))
--                                (mksequence [Assign "A" $ Val $ IntVal 100,
--                                             Throw (Op Plus (Var "X") (Var "Y")),
--                                             Assign "B" $ Val $ IntVal 200])
--                                Skip)
--                            "E"
--                            (Assign "Z" $ Op Plus (Var "E") (Var "A"))]

--testprog3 :: Statement
--testprog3 = mksequence [Assign "X" $ Val $ IntVal 0,
--                        Assign "Y" $ Val $ IntVal 1,
--                        Print "hello world: " $ Var "X",
--                        If (Op Lt (Var "X") (Var "Y")) (Assign "X" (Val (IntVal 3)))
--                                                       Skip,
--                        Assign "Z" $ Val $ IntVal 3]

--t7 :: Test
--t7 = run emptyStore testprog3 ~?=
--  (Map.fromList [("X", IntVal 3), ("Y",  IntVal 1), ("Z",IntVal 3)], Nothing, "hello world: 0", Nothing)

--testif :: Statement
--testif = If (Val (BoolVal True)) (Assign "X" (Val (IntVal 1))) (Assign "X" (Val (IntVal 0)))

--t6 :: Test
--t6 = step emptyStore testif ~?= 
--  (Map.empty, Nothing, "", Just $ Assign "X" (Val (IntVal 1)))


--t5 :: Test
--t5 = run emptyStore testprog2 ~?=
--   (Map.fromList [("A", IntVal 100), ("E", IntVal 1), ("X", IntVal 0), ("Y", IntVal 1), ("Z", IntVal 101)],
--    Nothing, "", Nothing)

--tests :: IO ()
--tests = do 
--   _ <- runTestTT $ TestList [ t1, t2, t3, t4, t5, t6, t7 ]
--   return ()

instance PP Store where
  pp = Map.foldrWithKey ppRow (PP.text "") where
    ppRow k v acc = (PP.text k <+> PP.char '=' <+> pp v) $$ acc

