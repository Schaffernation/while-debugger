module WhileParser where

import Control.Monad
import System.IO
import System.IO.Error

import Data.Map (Map)
import qualified Data.Map as Map

import ParserCombinators
import ParserTrans
import WhilePP

data Token = 
     TokVar String  Int  -- variables
   | TokVal Value   Int  -- primitive values
   | TokBop Bop     Int  -- binary operators
   | Keyword String Int  -- keywords
   | Literal String Int  -- String literals

      deriving (Eq, Show)
valueP :: GenParser Char Value
valueP = intP <|> boolP

intP :: GenParser Char Value
intP = do
  i <- int
  return $ IntVal i

newLineP :: GenParser Char Char 
newLineP = do
  c <- char '\n'
  incrLn
  return c

boolP :: GenParser Char Value
boolP = constP "true"  (BoolVal True) <|>
        constP "false" (BoolVal False)

opP :: GenParser Char Bop 
opP = constP "+"  Plus <|>
      constP "-"  Minus <|>
      constP "*"  Times <|>
      constP "/"  Divide <|>
      constP ">=" Ge <|>
      constP ">"  Gt <|>
      constP "<=" Le <|>
      constP "<"  Lt
      

varP :: GenParser Char Variable
varP = many1 upper

litP :: GenParser Char String
litP = between (char '"') (many $ satisfy ('"' /=)) (char '"')

type Lexer = GenParser Char [Token]

whileKeywords :: [ GenParser Char Token ]
whileKeywords = map insert keys where
  insert x = do
    ln <- getLn
    constP x (Keyword x ln) 
  keys     = [ "(", ")", ":=", ";", "if", "then", "else",
              "endif", "while", "do", "endwhile", "skip", "print" ]

whileLexer :: Lexer 
whileLexer = sepBy1
        (liftM2 TokVal valueP getLn <|>
         liftM2 TokVar varP getLn  <|>
         liftM2 TokBop opP getLn  <|>
         choice whileKeywords <|>
         liftM2 Literal litP getLn)
        (many (newLineP <|> space))

tokParenP :: GenParser Token a -> GenParser Token a
tokParenP p = do
  (Keyword "(" _) <- getC
  p'              <- p
  (Keyword ")" _) <- getC
  return p'

tokExprP :: GenParser Token Expression
tokExprP = sumP where
  sumP    = prodP   `chainl1` opLevel (level Plus)
  prodP   = compP   `chainl1` opLevel (level Times)
  compP   = factorP `chainl1` opLevel (level Gt)
  factorP = tokParenP tokExprP <|> baseP
  baseP   = do
    v <- getC
    case v of
      TokVal v' _ -> return $ Val v'
      TokVar v' _ -> return $ Var v'
      _           -> fail "not val or var"

-- only succeeds for operators at a particular precedence level
opLevel :: Int -> GenParser Token (Expression -> Expression -> Expression)
opLevel l = do
  x <- getC
  case x of
    TokBop bop _ -> if level bop == l then (return $ Op bop) else fail ""
    _ -> fail ""


tokStatementP :: GenParser Token Statement
tokStatementP = isSeq <|> notSeq where
  isSeq = do
    s1              <- notSeq
    (Keyword ";" _) <- getC
    s2              <- tokStatementP
    return $ Sequence s1 s2
  notSeq = isAss <|> isIf <|> isWhi <|> isSkp <|> isPrint where
    isAss = do
      (TokVar v _)     <- getC
      (Keyword ":=" ln) <- getC
      e                <- tokExprP
      return $ Assign v e ln
    isIf = do
      (Keyword "if" ln)    <- getC
      e                   <- tokExprP
      (Keyword "then" _)  <- getC
      s1                  <- tokStatementP
      (Keyword "else" _)  <- getC
      s2                  <- tokStatementP
      (Keyword "endif" _) <- getC
      return $ If e s1 s2 ln
    isWhi = do
      (Keyword "while"  ln)    <- getC
      e                      <- tokExprP
      (Keyword "do" _)       <- getC
      s                      <- tokStatementP
      (Keyword "endwhile" _) <- getC
      return $ While e s ln
    isSkp = do
      (Keyword "skip" ln) <- getC
      return $ Skip ln
    isPrint = do
      (Keyword "print" ln) <- getC
      (Literal s _)       <- getC
      e                   <- tokExprP
      return $ Print s e ln


doLexer :: String -> [Token]
doLexer s = case evalParse whileLexer s of
  t : ts -> t
  []     -> error "no parses"

tokParse :: String -> Either ParseError Statement
tokParse str = case doParse tokStatementP (doLexer str) of
    []      -> Left "No Parses"
    [(s,_)] -> Right s
    _       -> Left "Multiple Parses"

mkMap :: String -> Map Int String
mkMap s = m where
  (_, m) = foldl split (1, Map.empty) (lines s)
  split (ln, m) line = (ln + 1, Map.insert ln line m)

tokParseFromFile :: String -> IO (Either ParseError Statement, Map Int String)
tokParseFromFile filename = do 
  eHandle <- tryIOError (openFile filename ReadMode)
  case eHandle of
    Left _ -> return (Left "File Not Found", Map.empty)
    Right handle -> do
      str <- hGetContents handle
      return (tokParse str, mkMap str)

