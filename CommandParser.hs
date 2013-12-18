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
  | Man (Maybe String)
  | PrintLn (Maybe Int)
  | MkBreak Int
  | RmBreak Int
  | LsBreak
  | Exit
  | Error String
  deriving (Show, Eq)

data CmdToken = 
     InputStr String    
   | InputNum Int
   | Command String   -- keywords
   deriving (Show, Eq)

type CmdLexer = GenParser Char [CmdToken]

cmdstrs :: [(String, String, String)]
          -- name ----- usage ----------   description
cmdstrs = [ ("load",    "load filename",   "Loads a while program with the path *filename*"),
            ("step",    "step         ",   "Steps forward one line in the loaded program"), 
            ("back",    "back         ",   "Steps backward one execution step in the loaded program"),
            ("run",     "run          ",   "Runs the loaded program until the next breakpoint"),
            ("mkbreak", "mkbreak linenum", "Adds a breakpoint at *linenum*"),
            ("rmbreak", "rmbreak linenum", "Removes a breakpoint at *linenum*"),
            ("lsbreak", "lsbreak      ",   "Lists all the current breakpoints"),
            ("vars",    "vars         ",   "Prints the values of the variables currently assigned"),
            ("print",   "print /linenum/", "Prints the loaded program, or line number *linenum* of the loaded program"),
            ("help",    "help /command/",  "Prints the details of all commands, or the details of *command*"),
            ("exit",    "exit          ",  "Exits the debugger") ]

cmds :: [ GenParser Char CmdToken ]
cmds = map (\x -> constP x (Command x)) (map (\(n,_,_) -> n) cmdstrs)

cmdLexer :: CmdLexer
cmdLexer = sepBy1
   (choice cmds <|>
    liftM InputNum int <|>
    liftM InputStr strP ) (many space)

cmdP :: GenParser CmdToken Cmd
cmdP = help <|> load <|> intArg <|> noArg where
  help = do
    (Command "help") <- getC
    c'               <- getC
    return $ case c' of
      Command  c -> Man (Just c)
      InputStr c -> Man (Just c)
      _ -> Error errMessage
  load = do
    (Command "load") <- getC
    (InputStr file)  <- getC
    return $ Load file
  intArg = do
    c             <- getC
    (InputNum ln) <- getC
    return $ case c of 
      Command "print"   -> PrintLn $ Just ln
      Command "mkbreak" -> MkBreak ln
      Command "rmbreak" -> RmBreak ln
      _ -> Error errMessage
  noArg = do
    c <- getC
    return $ case c of 
      Command "step"    -> Step
      Command "back"    -> Back
      Command "vars"    -> Vars
      Command "run"     -> Run
      Command "lsbreak" -> LsBreak
      Command "print"   -> PrintLn Nothing
      Command "help"    -> Man Nothing
      Command "exit"    -> Exit
      _ -> Error errMessage
  errMessage = "Not a Command"


doCmdLexer :: String -> [CmdToken]
doCmdLexer s = case evalParse cmdLexer s of
  t : ts -> t
  []     -> []

cmdParse :: String -> Either ParseError Cmd
cmdParse str = case doParse cmdP (doCmdLexer str) of
    []      -> Left "No Parses"
    [(s,_)] -> Right s
    _       -> Left "Multiple Parses"
