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

printStmt :: Environment -> Maybe Statement -> IO ()
printStmt env mSt = do
  case mSt of 
    Nothing -> putStrLn "End of Program"
    Just st -> do
      lnMap <- readIORef (getLines env)
      let ln = getLn st
      case (Map.lookup ln lnMap) of
        Nothing   -> putStrLn $ (show ln) ++ " Hmm?"
        Just line -> putStrLn $ (show ln) ++ ": " ++ line

printProg :: Environment -> Maybe Statement -> IO ()
printProg env mSt = do
  lnMap <- readIORef (getLines env)
  putStr $ Map.foldrWithKey mkStrLn "" lnMap where
    ln = case mSt of 
      Nothing -> 0
      Just st -> getLn st
    mkStrLn ln' s s' = (if ln == ln' then "-> " else "   ") ++
      show ln' ++ ":\t" ++ s ++ "\n" ++ s'


execCmd :: Environment -> Cmd -> IO ()
execCmd env (Load file) = do
  (ioAst, lnMap) <- tokParseFromFile file
  case ioAst of
    Left _ -> putStrLn "not a valid file"
    Right ast -> do
      writeIORef (getNext env) (Just ast)
      writeIORef (getStore env) emptyStore
      writeIORef (getLines env) lnMap
      printProg env (Just ast)
execCmd env Step = do
  mNext <- readIORef (getNext  env)
  st    <- readIORef (getStore env)
  hist  <- readIORef (getHist  env)
  case mNext of
    Nothing -> putStrLn "No step"
    Just next -> do
      let (st', err, printLog, next') = step st next
      writeIORef (getNext env)  next'
      writeIORef (getStore env) st'
      writeIORef (getHist env)  ((next, st) : hist)
      printProg env next'
      putStr printLog
execCmd env Back = do
  hist <- readIORef (getHist env)
  case hist of
    [] -> putStrLn "No history"
    (next, st) : hist' -> do
      writeIORef (getNext env)  (Just next)
      writeIORef (getStore env) st
      writeIORef (getHist env)  hist'
      printProg env (Just next)
execCmd env Vars = do
  st <- readIORef (getStore env)
  putStr $ display st
execCmd env Run = do
  bps   <- readIORef (getBps env)
  st    <- readIORef (getStore env)
  mNext <- readIORef (getNext env)
  case mNext of
    Nothing -> putStrLn "Program Finished"
    Just next -> do
      let (st', err, printLog, next') = run bps st next
      writeIORef (getNext env)  next'
      writeIORef (getStore env) st'
      -- TODO: History for running
      --writeIORef (getHist env)  ((next, st) : hist)
      putStr printLog
      printProg env next'
execCmd env (MkBreak bp) = do
  bps <- readIORef (getBps env)
  writeIORef (getBps env) (insert bp bps)
execCmd env (RmBreak bp) = do
  bps <- readIORef (getBps env)
  writeIORef (getBps env) (delete bp bps)
execCmd env LsBreak = do
  bps <- readIORef (getBps env)
  putStrLn $ show bps
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