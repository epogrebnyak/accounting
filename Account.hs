module Account where 

import Prelude hiding (lookup)     
import Data.Map (fromList, toList, alter, lookup, Map, map, member)

data Category = Asset | Expense | Equity | Liability | Income | Profit deriving (Show, Eq)
data Side = Debit | Credit
sumSide :: Category -> Side
sumSide category = if category `elem` [Asset, Expense] then Debit else Credit
readCategory:: String -> Category
readCategory "asset" = Asset
readCategory "equity" = Equity
readCategory "liability" = Liability
readCategory "expense" = Expense
readCategory "income" = Income
readCategory c = error ("Wrong category: " ++ c)
type Amount = Float
data Account = Account Category [Amount] [Amount] deriving (Show, Eq)
type AccountName = String
type AccountMap = Map AccountName Account

data Pair = Pair { 
    whichAccountToDebit :: AccountName, 
    whichAccountToCredit :: AccountName
    }

nullAccount :: Category -> Account  
nullAccount category = Account category [] []    

-- упрощение просмотра 
account (Account t debits credits) = Account t [sum debits] [sum credits]

-- функции для изменения либо правой, либо левой стороны счета 
debit :: Amount -> Account -> Account
debit x (Account t debits credits) = Account t (x:debits) credits 
credit :: Amount -> Account -> Account
credit x (Account t debits credits) = Account t debits (x:credits)

-- рассчитываем баланс на одной или другой стороне 
balance :: Account -> Amount
balance (Account category debits credits) = case sumSide category of
    Debit -> s
    Credit -> -s 
    where s = sum(debits) - sum(credits) 

-- меняем один счет в словаре     
changeByKeyWith name func (Right accounts) = if member name accounts 
    then Right $ alter (fmap func) name accounts
    else Left $ "Account name not found: " ++ name
changeByKeyWith _ _ (Left accounts) = Left accounts


type ErrorMessage = String

applyEntry :: Either ErrorMessage AccountMap -> AccountName -> AccountName -> Amount -> Either ErrorMessage AccountMap 
applyEntry accountsM keyD keyC amount = f 
    where f = changeByKeyWith keyD (debit amount) $ changeByKeyWith keyC (credit amount) accountsM


n = Right $ fromList([("cash", nullAccount Asset),("paid-in capital", nullAccount Equity)])
s = Right $ fromList [("a", 1)]
w = applyEntry n "cash" "paid-in capital" 10    




d = changeByKeyWith "a" (+1)  s
-- d
-- Right (fromList [("a",2)])
         

-- change accounts amount func name = case member name accounts of
--     True -> Right (alter f' key accounts)
--     False -> Left ("Account name not found: " ++ name)
     
        




-- -- задача 1: прочитать из accountNames словарь типа AccountMap
-- --           accountNames может приходит как JSON
-- --           В результате мы получаем иницилизированную систему пустых счетов, 
-- --           где назания счетов - ключи словаря AccountMap


-- readAccountsByName :: [(String, [String])] -> AccountMap
-- readAccountsByName accountNamesJSON = fromList $ concatMap (uncurry f) accountNamesJSON
--     where f category names = zip names $ repeat (nullAccount (readCategory category))

-- accountNames = [ 
--     ("asset", ["cash", "receivable", "inventory"]),
--     ("equity", ["capital"]),
--     ("expense", ["cogs", "interest"]),
--     ("liability", ["payable", "debt"])
--     ]
-- -- Data.Tree

-- -- POSTGRES
-- -- sqlite-simple
-- -- Persist* / Yesod
-- -- Aeson

-- accountsExample = readAccountsByName accountNames



-- -- amount - smart конструктор

-- -- задача 4: уметь делать проводки - менять счета внутри AccountMap

-- data Entry = Entry AccountName AccountName Amount
-- mut accounts (Entry da ca amount) = mutate accounts (Pair da ca) amount


-- -- Примеры    
-- a = Account Asset [] []
-- z = balance $ credit 7 (debit 10 a) -- 3

-- -- a = Account Asset [100, 20] [70]
-- -- b = Account Equity [70] [100, 20]


-- e1 = Entry "cash" "capital" 100
-- e2 = Entry "cash" "debt" 2
-- -- accountsExample определен выше 
-- accounts1 = mut (mut accountsExample e1) e2


-- accounts' = fromList [("cash", nullAccount Asset), ("capital",nullAccount Equity)]
-- -- foldM
-- b = foldl mut accounts' [(Entry "cash" "capital" 100), -- взнос акционеров в капитал
--                          (Entry "cash" "capital" 20),  -- взнос акционеров в капитал
--                          (Entry "capital" "cash" 70)   -- выплата акционерам
--                         ] 
-- c = fromList [("capital",Account Equity [] [50]),
--               ("cash",Account Asset [50] [])]

-- -- в Quickcheck
-- -- Data.Map.map balance b == Data.Map.map balance c
-- -- True

