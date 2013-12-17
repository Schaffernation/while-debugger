--Syntax of WHILE programming language

module WhileSyntax where

import Control.Monad
import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP
import Parser hiding (get)
import ParserCombinators
import Test.HUnit hiding (State)
import Test.QuickCheck


data Variable = V String deriving (Eq, Ord, Show)

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
    deriving (Eq)

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v 
  | Op  Bop Expression Expression
    deriving (Eq)
             
data Bop = 
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Gt       -- > :: Int -> Int -> Bool 
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
    deriving (Eq)
             
data Value =
    IntVal  Int
  | BoolVal Bool
    deriving (Eq)              


data CmdResult = Statement Statement
               | Expression Expression
               | What

parseCmd  :: String -> CmdResult
parseCmd line = case doParse cmdP line of 
      [ (Right stmt,[]) ] -> Statement stmt
      [ (Left exp, []) ] -> Expression exp
      _ -> What

--------------------------------------------------
--------------------------------------------------
level :: Bop -> Int
level Plus   = 3
level Minus  = 3 
level Times  = 5
level _      = 8

-- Pretty printing and parsing for WHILE programming language

class PP a where
  pp :: a -> Doc

instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="

instance PP Value where
  pp (IntVal i)  = PP.int i 
  pp (BoolVal b) = if b then PP.text "true" else PP.text "false"


instance PP Expression where
  pp (Var x) = pp x
  pp (Val x) = pp x
  pp e@(Op _ _ _) = ppPrec 0 e  where
     ppPrec n (Op bop e1 e2) =
        parens (level bop < n) $
           ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2 
     ppPrec _ e' = pp e'
     parens b = if b then PP.parens else id


instance PP Variable where
  pp (V x) = PP.text x

instance PP Statement where
  pp (Assign x e) = pp x <+> PP.text ":=" <+> pp e
  pp (If e s1 s2) = 
    PP.vcat [PP.text "if" <+> pp e <+> PP.text "then",
         PP.nest 2 (pp s1), 
         PP.text "else",
         PP.nest 2 (pp s2),
         PP.text "endif"]

  pp (While e s)  = 
     PP.vcat [PP.text "while" <+> pp e <+> PP.text "do",
              PP.nest 2 (pp s),
              PP.text "endwhile"]            
  pp (Sequence s1@(Sequence _ _) s2) = PP.parens (pp s1) <> PP.semi $$ pp s2     
  pp (Sequence s1 s2) = pp s1 <> PP.semi $$ pp s2
  pp Skip = PP.text "skip"

display :: PP a => a -> String
display = show . pp

-- Use pretty printer for showing everything

instance Show Value where
  show = display
instance Show Bop where  
  show = display
instance Show Expression where  
  show = display
instance Show Statement where
  show = display


{-
-- to see the AST instead of pretty printing, 
-- replace the above with the following code.
deriving instance Show Variable
deriving instance Show Value
deriving instance Show Bop
deriving instance Show Expression
deriving instance Show Statement
-}

----------------------------------------------------
--- Parsing

-- parse something, consuming any whitespace that follows
wsP :: Parser a -> Parser a
wsP p = do x <- p 
           many space             
           return x

-- parse a string, consuming any whitespace that follows and 
-- return the constant
constP :: String -> a -> Parser a
constP s x = string s >> many space >> return x 

-- every parser after this point consumes all following whitespace

opP :: Parser Bop 
opP =   constP "+" Plus 
    <|> constP "-" Minus
    <|> constP "*" Times
    <|> constP ">=" Ge
    <|> constP "<=" Le
    <|> constP ">" Gt
    <|> constP "<" Lt

varP :: Parser Variable
varP = liftM V $ wsP (many1 upper)

valueP :: Parser Value
valueP = (wsP intP) <|> boolP where
    intP = liftM IntVal int
    boolP =  constP "true" (BoolVal True) 
         <|> constP "false" (BoolVal False)


-- parse something between open and close parentheses, consuming
-- any additional whitespace
parenP :: Parser a -> Parser a 
parenP p = between (wsP (char '(')) p (wsP (char ')'))

varExprP = do x <- wsP varP;   return (Var x)
valExprP = do x <- wsP valueP; return (Val x)


-- use chainl1 for associativity and precedence
exprP :: Parser Expression
exprP = sumP where
   sumP    = prodP   `chainl1` opLevel (level Plus)
   prodP   = compP   `chainl1` opLevel (level Times)
   compP   = factorP `chainl1` opLevel (level Gt)
   factorP = parenP exprP <|> baseP
   baseP   = liftM Val valueP <|> liftM Var varP


-- only succeeds for operators at a particular precedence level
opLevel :: Int -> Parser (Expression -> Expression -> Expression)
opLevel l = do x <- opP 
               if level x == l then (return $ Op x) else fail ""

-- parse a string and consume any following whitespace
sstring :: String -> Parser ()
sstring s = constP s () 

statementP :: Parser Statement
statementP = do s1 <- baseP 
                seqP s1 <|> return s1  where

  seqP s1  = do sstring ";" 
                s2 <- statementP 
                return (Sequence s1 s2)

  baseP   = assignP <|> ifP <|> whileP <|> skipP <|> factorP

  assignP = do  x <- varP
                sstring ":="
                e <- exprP
                many space
                return (Assign x e)
  
  ifP     = do  sstring "if"
                e <- exprP
                sstring "then"
                s1 <- statementP
                sstring "else"
                s2 <- statementP
                sstring "endif"
                return (If e s1 s2)

  whileP  = do  sstring "while"
                e <- exprP
                sstring "do"
                s <- statementP
                sstring "endwhile"
                return (While e s)

  skipP   = do  sstring "skip"
                return Skip   
                
  factorP = parenP statementP 

----------------------------------------------------

cmdP :: Parser (Either Expression Statement)
cmdP = (sstring "eval" >> liftM Left exprP) <|> liftM Right statementP

            
-------------------------------------------------         

-- round trip property for expressions
prop_RTE :: Expression -> Bool
prop_RTE s = case doParse exprP (display s) of 
              [ (s', []) ] -> s == s'
              _  -> False

-- round trip property for statements          
prop_RTS :: Statement -> Bool
prop_RTS s = case doParse statementP (display s) of 
              [ (s', []) ] -> s == s'
              _  -> False          




-------------------------------------------------         

arbVar :: Gen Variable
arbVar = do x <- elements ['A'..'Z']
            return $ V [x]

arbValue :: Gen Value
arbValue    = oneof [ liftM IntVal arbitrary
                    , liftM BoolVal arbitrary ]

arbBop,arbArithBop :: Gen Bop
arbArithBop = elements [ Plus, Times, Minus ]
arbBop      = elements [ Plus, Times, Minus, Gt, Ge, Lt, Le]



-- generating arbitrary expressions
-- given a generation function for the boolean operators
-- and a (decreasing) size
arbnE :: Int -> Gen Expression
arbnE n = frequency [ (1, liftM Var arbVar)
                        , (1, liftM Val arbValue)
                        , (n, liftM3 Op arbBop (arbnE n_by_2) (arbnE n_by_2))]
  where n_by_2 = n `div` 2


-- generating statements, parameterized by expression generator
arbnS' arbE n = frequency [ (1, liftM2 Assign arbVar (arbE n_by_2))
                          , (1, return Skip)
                          , (n, liftM3 If (arbE n_by_2)    (arbnS' arbE n_by_2) (arbnS' arbE n_by_2))
                          , (n, liftM2 Sequence   (arbnS' arbE n_by_2) (arbnS' arbE n_by_2)) 
                          , (n, liftM2 While (arbE n_by_2) (arbnS' arbE n_by_2))  
                     ]
    where n_by_2 = n `div` 2


-- any expressions
arbnS n  = arbnS' arbnE n

instance Arbitrary Expression where
   arbitrary = sized arbnE
   shrink (Op b e1 e2) = [e1, e2] 
            ++ [Op b e1' e2 | e1' <- shrink e1] 
            ++ [Op b e1 e2' | e2' <- shrink e2]
   shrink _ = []

instance Arbitrary Statement where
   arbitrary = sized arbnS
   shrink (Assign v e) = map (Assign v) (shrink e)
   shrink (Sequence s1 s2) = [s1, s2] ++
    [Sequence s1' s2 | s1' <- shrink s1] ++
    [Sequence s1  s2' | s2' <- shrink s2]
   shrink (If e s1 s2) =  [s1, s2] ++
    [If e' s1 s2 | e' <- shrink e] ++
    [If e s1' s2 | s1' <- shrink s1] ++
    [If e s1  s2' | s2' <- shrink s2]
   shrink (While e s1) = [s1] ++
    [While e' s1 | e' <- shrink e] ++
    [While e s1' | s1' <- shrink s1]
   shrink Skip = []

instance Arbitrary Value where 
   arbitrary = arbValue

instance Arbitrary Variable where
   arbitrary = arbVar
