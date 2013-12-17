{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
FlexibleInstances #-}

module Main where

import Data.IORef
import System.IO

import WhileParser
import WhilePP
import WhileStep
import CommandParser

type Environment = (IORef [Int], 
                    IORef (Maybe Statement),
                    IORef Store,
                    IORef [(Statement, Store)])

getBps :: Environment -> IORef [Int]
getBps (bps, _, _, _) = bps

getNext :: Environment -> IORef (Maybe Statement)
getNext (_, n, _, _) = n

getStore :: Environment -> IORef Store
getStore (_, _, st, _) = st

getHist :: Environment -> IORef [(Statement, Store)]
getHist (_, _, _, hist) = hist

execCmd :: Environment -> Cmd -> IO ()
execCmd env (Load file) = do
  ioAst <- tokParseFromFile file
  case ioAst of
    Left _ -> putStrLn "not a valid file"
    Right ast -> do
      writeIORef (getNext env) (Just ast)
      writeIORef (getStore env) emptyStore
execCmd env Step = do
  mNext <- readIORef (getNext env)
  st    <- readIORef (getStore env)
  hist  <- readIORef (getHist env)
  case mNext of
    Nothing -> putStrLn "No step"
    Just next -> do
      let (st', err, printLog, next') = step st next
      writeIORef (getNext env)  next'
      writeIORef (getStore env) st'
      writeIORef (getHist env)  ((next, st) : hist)
      putStr printLog
execCmd env Back = do
  hist <- readIORef (getHist env)
  case hist of
    [] -> putStrLn "No history"
    (next, st) : hist' -> do
      writeIORef (getNext env)  (Just next)
      writeIORef (getStore env) st
      writeIORef (getHist env)  hist'
execCmd env Vars = do
  st <- readIORef (getStore env)
  putStrLn $ display st

execCmd env _ = undefined

parseLine :: Environment -> String -> IO ()
parseLine env line = case cmdParse line of
  Left _ -> return ()
  Right cmd -> execCmd env cmd

main :: IO ()
main = do
  bps  <- newIORef []
  next <- newIORef Nothing
  vmap <- newIORef emptyStore
  hist <- newIORef []
  repl (bps, next, vmap, hist)


repl :: Environment -> IO ()
repl env = do
   putStr "%> "
   hFlush stdout
   line <- getLine
   parseLine env line
   repl env