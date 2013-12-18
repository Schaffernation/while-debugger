{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module ParserTrans (GenParser, Parser, 
                   getC,
                   getLn,
                   incrLn,
                   choose,
                   (<|>),
                   satisfy,
                   doParse,
                   evalParse  
                   ) where


import Control.Monad.State

type GenParser e a = StateT ([e], Int) [] a

type Parser a = GenParser Char a

start :: Int
start = 1

doParse :: GenParser e a  -> [e] -> [(a,[e])]
doParse p s = map (\(val, rest) -> (val, fst rest)) (doParseLines p (s, start))

doParseLines :: GenParser e a  -> ([e], Int) -> [(a,([e], Int))]
doParseLines p s = runStateT p s

evalParse :: GenParser e a -> [e] -> [a]
evalParse p s = evalStateT p (s, start)

-- | Return the next character
getC :: GenParser e e 
getC = do
  (input, ln) <- get
  case input of
    (x : xs) -> do
      put (xs, ln)
      return x
    []       -> fail "End of input"

getLn :: GenParser e Int
getLn = do
  (_, ln) <- get
  return ln

incrLn :: GenParser e ()
incrLn = do
  (input, ln) <- get
  put (input, ln + 1)

-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (e -> Bool) -> GenParser e e 
satisfy p = do c <- getC
               if (p c) then return c else fail "End of input"

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: GenParser e a -> GenParser e a -> GenParser e a
choose p1 p2 = StateT (\cs -> doParseLines p1 cs ++ doParseLines p2 cs)

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: GenParser e a -> GenParser e a -> GenParser e a
p1 <|> p2 = StateT $ \cs -> case doParseLines (p1 `choose` p2) cs of
                          []   -> []
                          x:_ -> [x]


