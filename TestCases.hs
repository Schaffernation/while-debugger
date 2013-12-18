import Control.Monad
import Test.QuickCheck
import Test.HUnit hiding (State)


import WhileParser
import WhilePP
import WhileStep

arbVar :: Gen String
arbVar = fmap (:[]) $ Test.QuickCheck.choose ('A','Z')

arbEI :: Gen Expression
arbEI = sized arbnEI 
  where arbnEI 0 = oneof [ liftM Var arbVar
                         , liftM (Val . IntVal) arbitrary ]
        arbnEI n = oneof [ liftM Var arbVar
                         , liftM (Val . IntVal) arbitrary
                         , liftM2 (Op Plus)   (arbnEI n_by_2) (arbnEI n_by_2) 
                         , liftM2 (Op Times)  (arbnEI n_by_2) (arbnEI n_by_2) 
                         , liftM2 (Op Minus)  (arbnEI n_by_2) (arbnEI n_by_2) 
                         ]
                   where n_by_2 = n `div` 2

arbEB :: Gen Expression
arbEB = oneof [ liftM (Val . BoolVal) arbitrary
              , liftM2 (Op Gt) arbEI arbEI
              , liftM2 (Op Ge) arbEI arbEI
              , liftM2 (Op Lt) arbEI arbEI
              , liftM2 (Op Le) arbEI arbEI ]

arbS :: Gen Statement
arbS = liftM rb (sized arbnS) 
  where arbnS 0 = oneof [ return $ Skip 1
                        , liftM2 (\v e -> Assign v e 1) arbVar arbEI ]
        arbnS n = oneof [ liftM3 (\b s1 s2 -> If b s1 s2 1) arbEB (arbnS n_by_2) (arbnS n_by_2)
                        , liftM2 (\b s -> While b s 1) arbEB (arbnS n_by_2)
                        , liftM2 Sequence (arbnS n_by_2) (arbnS n_by_2) 
                        ]
                   where n_by_2 = n `div` 2


instance Arbitrary Statement where
  arbitrary = arbS   
  shrink s@(Sequence s1 s2) = [s1, s2]
  shrink s@(While _ s1 _)   = [s1]
  shrink s@(If _ s1 s2 _)   = [s1, s2]
  shrink (Skip _) = []
  shrink s = [Skip 1]


prop_RT :: Statement -> Bool
prop_RT s = case tokParse (display s) of
  Left _   -> False
  Right s' -> s == s'


----------------------------

breakpointTest :: Test
breakpointTest =
  let (_, _, _, next) = run [2] emptyStore (Sequence (Skip 1) (Skip 2)) in
  next ~?= Just (Skip 2)

runTest :: Test
runTest =
  let (_, _, _, next) = run [] emptyStore (Sequence (Skip 1) (Skip 2)) in
  next ~?= Nothing

stepTest :: Test
stepTest =
  let (_, _, _, next) = step emptyStore (Sequence (Skip 1) (Skip 2)) in
  next ~?= Just (Skip 2)

ifStepTest :: Test
ifStepTest =
  let (_, _, _, next) = step emptyStore (If (Val $ BoolVal True) (Skip 2) (Skip 3) 1) in
  next ~?= Just (Skip 2)

ifStepTest2 :: Test
ifStepTest2 =
  let (_, _, _, next) = step emptyStore (If (Val $ BoolVal False) (Skip 2) (Skip 3) 1) in
  next ~?= Just (Skip 3)

errorTest :: Test
errorTest = let (_, err, _, _) = run [] emptyStore (Assign "X" (Op Plus (Val $ IntVal 1) (Val $ BoolVal True)) 1) in
  err ~?= Just "Binary Operation expected integers at line 1"

errorTest2 :: Test
errorTest2 = let (_, err, _, _) = run [] emptyStore (Assign "X" (Var "Y") 1) in
  err ~?= Just "Variable not in scope at line 1"

tests :: Test
tests = TestList [breakpointTest, runTest, stepTest, ifStepTest, ifStepTest2,
  errorTest, errorTest2]