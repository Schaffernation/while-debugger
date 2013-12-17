{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- CIS 552, University of Pennsylvania
-- based on Parsec and ReadP parsing libraries
module ParserCombinators where

import ParserTrans
import Control.Monad
import Data.Char
import System.IO

            
type ParseError = String

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors.
-- However, for compatibility with Parsec, we give this function 
-- the same type.
parse :: GenParser s a -> [s] -> Either ParseError a
parse parser str = case (doParse parser str) of 
    []      -> Left  "No parses"
    [(a,_)] -> Right a
    _       -> Left  "Multiple parses"
    
parseFromFile :: GenParser Char a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do 
  handle <- openFile filename ReadMode 
  str <- hGetContents handle
  return $ parse parser str   
  
-- | Parsers for specific sorts of characters 
alpha, digit, upper, lower, space :: GenParser Char Char
alpha    = satisfy isAlpha
alphaNum = satisfy isAlphaNum
digit    = satisfy isDigit            
upper    = satisfy isUpper
lower    = satisfy isLower
space    = satisfy isSpace
   
-- | Parses and returns the specified character        
-- succeeds only if the input is exactly that character
char :: Char -> GenParser Char Char
char c = satisfy (c ==)   

-- | Parses and returns the specified string. 
-- Succeeds only if the input is the given string
string :: String -> GenParser Char String
string = mapM char

-- | succeed only if the input is a (positive or negative) integer
int :: GenParser Char Int
int = do n <- string "-" <|> return []
         s <- many1 digit  
         return $ (read (n ++ s) :: Int)

-- | given a parser, apply it as many times as possible                         
-- and return the answer in a list
many   :: GenParser s a -> GenParser s [a]
many p = many1 p <|> many0 
   where many0 = return []
                    
-- | given a parser, apply it as many times as possible,
-- but at least once.
many1 :: GenParser s a -> GenParser s [a]
many1 p = do x  <- p
             xs <- many p
             return (x:xs)

-- | @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl :: GenParser s b -> GenParser s (b -> b -> b) -> b -> GenParser s b
chainl p op x = chainl1 p op <|> return x

-- | Like 'chainl', but parses one or more occurrences of @p@.
chainl1 :: GenParser s a -> GenParser s (a -> a -> a) -> GenParser s a
p `chainl1` pop = p >>= rest
    where rest x = next x <|> return x 
          next x = do o <- pop
                      y <- p
                      rest $ x `o` y 
                      
                      
-- | Combine all parsers in the list (sequentially)
choice :: [GenParser s a] -> GenParser s a
choice = foldr (<|>) (fail "")

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between :: GenParser s open -> GenParser s a -> GenParser s close -> GenParser s a
between open p close = do _ <- open
                          x <- p
                          _ <- close
                          return x

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: GenParser s a -> GenParser s sep -> GenParser s [a]
sepBy p sep = sepBy1 p sep <|> return []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: GenParser s a -> GenParser s sep -> GenParser s [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))


-- Lexer ------------------------------------------------------------

constP :: String -> a -> GenParser Char a
constP s x = do
  s' <- string s
  return x

wsP :: GenParser Char a -> GenParser Char a
wsP p = do
  x <- p
  _ <- many space
  return x

strP :: GenParser Char String
strP =  many1 ( alphaNum <|> char '.' <|> char '_' <|> char '/')

