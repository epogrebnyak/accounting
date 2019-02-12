-- -- TODO: перенести в тесты
-- -- получаем Right
-- s = asAccountMap [("a", nullAccount Asset)]
-- d = debitByKey "a" 1  s -- Right (fromList [("a",2)])
-- -- получаем Left
-- d' = creditByKey "q" 1 s


-- file SpecAccount.hs
module SpecAccount where 

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Account (balance, credit, debit, nullAccount, Account(..), Category(..))

main :: IO ()
main = hspec $ do
    describe "Account.hs" $ do
        it "mutates the account debit and credit" $ property $ 
          (\a b -> ((a-b)::Float) == ((balance . credit b . debit a) (nullAccount Asset))) 
        
        it "appreciates debit and credit side balances" $ do
        balance (Account Asset [100, 20] [70]) `shouldBe` (balance (Account Equity [70] [100, 20]))

        --it "returns the first element of a list" $ do
        --head [23 ..] `shouldBe` (23 :: Int)
    
        --it "returns the first element of an *arbitrary* list" $
        --property $ \x xs -> head (x:xs) == (x :: Int)
    
        --it "throws an exception if used with an empty list" $ do
        --evaluate (head []) `shouldThrow` anyException



