{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
FlexibleInstances #-}

module Main where

import Data.IORef
import System.IO

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import WhileParser
import WhilePP
import WhileStep
import CommandParser

type LineMap = Map Line String

type Environment = (IORef [Int], 
                    IORef (Maybe Statement),
                    IORef Store,
                    IORef [(Statement, Store)],
                    IORef LineMap)

getBps :: Environment -> IORef [Int]
getBps (bps, _, _, _, _) = bps

getNext :: Environment -> IORef (Maybe Statement)
getNext (_, n, _, _, _) = n

getStore :: Environment -> IORef Store
getStore (_, _, st, _, _) = st

getHist :: Environment -> IORef [(Statement, Store)]
getHist (_, _, _, hist, _) = hist

getLines :: Environment -> IORef LineMap
getLines (_, _, _, _, lineMap) = lineMap

printIfLoaded :: Environment -> String -> IO ()
printIfLoaded env str = do
  lnMap <- readIORef (getLines env)
  putStrLn $ if null (Map.keys lnMap) 
    then "No Program Loaded"
    else str


printLn :: Environment -> Line -> IO ()
printLn env ln = do
  lnMap <- readIORef (getLines env)
  mNext <- readIORef (getNext env)
  printIfLoaded env $ case Map.lookup ln lnMap of
    Nothing   -> "Line does not exist"
    Just line -> margin ++ show ln ++ ":\t" ++ line where
      margin = case mNext of
        Nothing -> "   "
        Just next -> if getLn next == ln then "-> " else "   "


printStmt :: Environment -> Maybe Statement -> IO ()
printStmt env mSt = do
  case mSt of 
    Nothing -> return ()
    Just st -> do
      let ln = getLn st
      printLn env ln

printProg :: Environment -> Maybe Statement -> IO ()
printProg env mSt = do
  lnMap <- readIORef (getLines env)
  printIfLoaded env (init $ Map.foldrWithKey mkStrLn "" lnMap) where
    margin = case mSt of 
      Nothing -> const "   "
      Just st -> \ln -> if getLn st == ln then "-> " else "   "
    mkStrLn ln s s' =  margin ln ++ show ln ++ ":\t" ++ s ++ "\n" ++ s'

execCmd :: Environment -> Cmd -> IO ()
execCmd env (Load file) = do
  (ioAst, lnMap) <- tokParseFromFile file
  case ioAst of
    Left e -> putStrLn e
    Right ast -> do
      writeIORef (getNext env) (Just ast)
      writeIORef (getStore env) emptyStore
      writeIORef (getLines env) lnMap
      writeIORef (getBps env) []
      printProg env (Just ast)
execCmd env Step = do
  mNext <- readIORef (getNext  env)
  st    <- readIORef (getStore env)
  hist  <- readIORef (getHist  env)
  case mNext of
    Nothing -> putStrLn "End of Program"
    Just next -> do
      let (st', err, printLog, next') = step st next
      writeIORef (getNext env)  next'
      writeIORef (getStore env) st'
      writeIORef (getHist env)  ((next, st) : hist)
      putStr printLog
      case err of
        Nothing  -> printStmt env next'
        Just msg -> putStrLn $ "Error: " ++ msg
execCmd env Back = do
  hist <- readIORef (getHist env)
  case hist of
    [] -> putStrLn "No history"
    (next, st) : hist' -> do
      writeIORef (getNext env)  (Just next)
      writeIORef (getStore env) st
      writeIORef (getHist env)  hist'
      printStmt env (Just next)
execCmd env Vars = do
  st <- readIORef (getStore env)
  putStr $ display st
execCmd env Run = do
  bps   <- readIORef (getBps env)
  st    <- readIORef (getStore env)
  hist  <- readIORef (getHist  env)
  mNext <- readIORef (getNext env)
  case mNext of
    Nothing -> putStrLn "End of Program"
    Just next -> do
      let (st', err, printLog, next') = run bps st next
      writeIORef (getNext env)  next'
      writeIORef (getStore env) st'
      writeIORef (getHist env)  ((next, st) : hist)
      putStr printLog
      case err of
        Nothing  -> printStmt env next'
        Just msg -> putStrLn $ "Error: " ++ msg
execCmd env (PrintLn mLn) = do
  mNext <- readIORef (getNext env)
  case mLn of 
    Nothing -> printProg env mNext
    Just ln -> printLn env ln
execCmd env (MkBreak bp) = do
  bps <- readIORef (getBps env)
  writeIORef (getBps env) (insert bp bps)
execCmd env (RmBreak bp) = do
  bps <- readIORef (getBps env)
  writeIORef (getBps env) (delete bp bps)
execCmd env LsBreak = do
  bps <- readIORef (getBps env)
  putStrLn $ if null bps then "No Breakpoints" else "Breakpoints: " ++ show bps
execCmd _   (Error str) = putStrLn str
execCmd env (Man mCmd)  = do
  let
    printCmd        = putStrLn . formatList
    justCmd cmd     = filter (\(cmd',_,_) -> isInfixOf cmd cmd')
    formatList []   = "No Such Command"
    formatList strs = foldr1 (\t ts -> t ++ '\n' : ts) (map format strs)
    format (_, usage, description) = usage ++ "\t : " ++ description
  case mCmd of
    Nothing  -> printCmd cmdstrs
    Just cmd -> printCmd $ justCmd cmd cmdstrs where 
--execCmd env _ = undefined

parseLine :: Environment -> String -> IO ()
parseLine env line = case cmdParse line of
  Left _ -> return ()
  Right cmd -> execCmd env cmd

main :: IO ()
main = do
  bps   <- newIORef []
  next  <- newIORef Nothing
  vmap  <- newIORef emptyStore
  hist  <- newIORef []
  lnMap <- newIORef Map.empty
  repl (bps, next, vmap, hist, lnMap)


repl :: Environment -> IO ()
repl env = do
   putStr "%> "
   hFlush stdout
   line <- getLine
   parseLine env line
   repl env