module CommandParser where

import Control.Monad

import ParserCombinators
import ParserTrans

data Cmd =
    Load String
  | Step
  | Back
  | Vars
  | Run
  | MkBreak Int
  | RmBreak Int
  | LsBreak
  deriving (Show, Eq)

data CmdToken = 
     InputStr String    
   | InputNum Int
   | Command String   -- keywords

type CmdLexer = GenParser Char [CmdToken]

cmds :: [ GenParser Char CmdToken ]
cmds = map (\x -> constP x (Command x)) 
          [ "load", "step", "back", "run", "mkbreak", "rmbreak", "lsbreak", 
            "vars" ]

cmdLexer :: CmdLexer
cmdLexer = sepBy1
   (choice cmds <|>
    liftM InputNum int <|>
    liftM InputStr strP ) (many space)

cmdP :: GenParser CmdToken Cmd
cmdP = load <|> cmdBreak <|> noArg where
  load = do
    (Command "load") <- getC
    (InputStr file)  <- getC
    return $ Load file
  cmdBreak = do
    c             <- getC
    (InputNum ln) <- getC
    case c of 
      Command "mkbreak" -> return $ MkBreak ln
      Command "rmbreak" -> return $ RmBreak ln
      _ -> fail "nope"
  noArg = do
    c <- getC
    case c of 
      Command "step"    -> return Step
      Command "back"    -> return Back
      Command "vars"    -> return Vars
      Command "run"     -> return Run
      Command "lsbreak" -> return LsBreak
      _ -> fail "nope"


doCmdLexer :: String -> [CmdToken]
doCmdLexer s = case evalParse cmdLexer s of
  t : ts -> t
  []     -> []

cmdParse :: String -> Either ParseError Cmd
cmdParse str = case doParse cmdP (doCmdLexer str) of
    []      -> Left "No Parses"
    [(s,_)] -> Right s
    _       -> Left "Multiple Parses"
