module WhileParser where

import Control.Monad
import System.IO

import ParserCombinators
import ParserTrans
import WhilePP

data Token = 
     TokVar String       -- variables
   | TokVal Value        -- primitive values
   | TokBop Bop          -- binary operators
   | Keyword String      -- keywords
   | Literal String      -- String literals

      deriving (Eq, Show)
valueP :: GenParser Char Value
valueP = intP <|> boolP

intP :: GenParser Char Value
intP = do
  i <- int
  return $ IntVal i

boolP :: GenParser Char Value
boolP = constP "true"  (BoolVal True) <|>
        constP "false" (BoolVal False)

opP :: GenParser Char Bop 
opP = constP "+"  Plus <|>
      constP "-"  Minus <|>
      constP "*"  Times <|>
      constP "/"  Divide <|>
      constP ">"  Gt <|>
      constP ">=" Ge <|>
      constP "<"  Lt <|>
      constP "<=" Le
      

varP :: GenParser Char Variable
varP = many1 upper

litP :: GenParser Char String
litP = between (char '"') (many $ satisfy ('"' /=)) (char '"')

type Lexer = GenParser Char [Token]

whileKeywords :: [ GenParser Char Token ]
whileKeywords = map (\x -> constP x (Keyword x)) 
             [ "(", ")", ":=", ";", "if", "then", "else",
             "endif", "while", "do", "endwhile", "skip", "print" ]

whileLexer :: Lexer 
whileLexer = sepBy1
        (liftM TokVal valueP <|>
         liftM TokVar varP   <|>
         liftM TokBop opP    <|>
         choice whileKeywords <|>
         liftM Literal litP)
        (many space)

tokParenP :: GenParser Token a -> GenParser Token a
tokParenP p = do
  (Keyword "(") <- getC
  p'            <- p
  (Keyword ")") <- getC
  return p'

tokExprP :: GenParser Token Expression
tokExprP = op <|> valVar <|> tokParenP tokExprP where
  op = do
    e1         <- tokParenP tokExprP <|> valVar
    (TokBop b) <- getC
    e2         <- tokExprP <|> tokParenP tokExprP
    return $ Op b e1 e2
  valVar = do
        v <- getC
        case v of
          TokVal v' -> return $ Val v'
          TokVar v' -> return $ Var v'
          _         -> fail "not val or var"

tokStatementP :: GenParser Token Statement
tokStatementP = isSeq <|> notSeq where
  isSeq = do
    s1            <- notSeq
    (Keyword ";") <- getC
    s2            <- tokStatementP
    return $ Sequence s1 s2
  notSeq = isAss <|> isIf <|> isWhi <|> isSkp <|> isPrint where
    isAss = do
      (TokVar v)     <- getC
      (Keyword ":=") <- getC
      e              <- tokExprP
      return $ Assign v e
    isIf = do
      (Keyword "if")    <- getC
      e                 <- tokExprP
      (Keyword "then")  <- getC
      s1                <- tokStatementP
      (Keyword "else")  <- getC
      s2                <- tokStatementP
      (Keyword "endif") <- getC
      return $ If e s1 s2
    isWhi = do
      (Keyword "while")    <- getC
      e                    <- tokExprP
      (Keyword "do")       <- getC
      s                    <- tokStatementP
      (Keyword "endwhile") <- getC
      return $ While e s
    isSkp = do
      (Keyword "skip") <- getC
      return Skip
    isPrint = do
      (Keyword "print") <- getC
      (Literal s)       <- getC
      e                 <- tokExprP
      return $ Print s e


doLexer :: String -> [Token]
doLexer s = case evalParse whileLexer s of
  t : ts -> t
  []     -> error "no parses"

tokParse :: String -> Either ParseError Statement
tokParse str = case doParse tokStatementP (doLexer str) of
    []      -> Left "No Parses"
    [(s,_)] -> Right s
    _       -> Left "Multiple Parses"

tokParseFromFile :: String -> IO (Either ParseError Statement)
tokParseFromFile filename = do 
  handle <- openFile filename ReadMode 
  str <- hGetContents handle
  return $ tokParse str
