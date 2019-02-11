module Account where 

import Prelude hiding (lookup, map)     
import Data.Map (fromList, toList, alter, lookup, Map, map, member)

data Category = Asset | Expense | Equity | Liability | Income | Profit deriving (Show, Eq)
data Side = Debit | Credit
sumSide :: Category -> Side
sumSide category = if category `elem` [Asset, Expense] then Debit else Credit
type Amount = Float
data Account = Account Category [Amount] [Amount] deriving (Show, Eq)
type AccountName = String
type AccountMap = Map AccountName Account

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

accountNames = [ 
        ("asset", ["cash", "receivable", "inventory"]),
        ("equity", ["capital"]),
        ("expense", ["cogs", "interest"]),
        ("liability", ["payable", "debt"])
        ]
readCategory:: String -> Maybe Category
readCategory "asset" = Just Asset 
readCategory "equity" = Just Equity
readCategory "liability" = Just Liability
readCategory "expense" = Just Expense
readCategory "income" = Just Income
readCategory _ = Nothing

readTuple :: (String, [String]) -> [(AccountName, Account)]
readTuple (categoryStr, names) = maybe [] zip' (readCategory categoryStr)
    where zip' category = zip names (repeat (nullAccount category))

readAccountsByName :: [(String, [String])] -> AccountMap
readAccountsByName source = fromList $ concatMap readTuple source

accountsExample = Right $ readAccountsByName accountNames
    
-- меняем один счет в словаре     
changeByKeyWith name f (Right accounts) = if member name accounts 
    then Right $ alter (fmap f) name accounts
    else Left $ "Account name not found: " ++ name
changeByKeyWith _ _ (Left accounts) = Left accounts

type ErrorMessage = String
data Entry = Entry AccountName AccountName Amount

applyEntry :: Either ErrorMessage AccountMap -> Entry -> Either ErrorMessage AccountMap
applyEntry accountsM (Entry key1 key2 amount) = f $ g accountsM
    where 
        f = changeByKeyWith key1 (debit amount) 
        g = changeByKeyWith key2 (credit amount) 
applyEntries = foldl applyEntry  
mut = applyEntry 
muts = applyEntries 

n = Right $ fromList([("cash", nullAccount Asset),("paid-in capital", nullAccount Equity)])
w = applyEntry n $ Entry "cash" "paid-in capital" 10    

s = Right $ fromList [("a", 1)]
d = changeByKeyWith "a" (+1)  s
-- d
-- Right (fromList [("a",2)])
d' = changeByKeyWith "q" (+1) s
         
-- -- Примеры    
a = Account Asset [] []
z = balance $ credit 7 (debit 10 a) -- 3

g = Account Asset [100, 20] [70]
e = Account Equity [70] [100, 20]

e1 = Entry "cash" "capital" 100
e2 = Entry "cash" "debt" 400
accounts1 = mut (mut accountsExample e1) e2
accounts1' = foldl mut accountsExample [e1, e2]

accounts' = Right $ fromList [("cash", nullAccount Asset), ("capital",nullAccount Equity)]
b' = muts accounts' [(Entry "cash" "capital" 100), -- взнос акционеров в капитал
                     (Entry "cash" "capital" 20),  -- взнос акционеров в капитал
                     (Entry "capital" "cash" 70)   -- выплата акционерам
                     ] 
c' = Right $ fromList [("capital",Account Equity [] [50]),
                       ("cash",Account Asset [50] [])]

-- -- в Quickcheck
balances :: Either ErrorMessage AccountMap -> Either ErrorMessage (Map AccountName Amount)
balances = fmap (Data.Map.map balance)
-- balances b' == balances c'
-- True

-- -- задача 1: прочитать из accountNames словарь типа AccountMap
-- --           accountNames может приходит как JSON
-- --           В результате мы получаем иницилизированную систему пустых счетов, 
-- --           где назания счетов - ключи словаря AccountMap

-- -- Data.Tree
-- -- POSTGRES
-- -- sqlite-simple
-- -- Persist* / Yesod
-- -- Aeson
-- -- smart конструктор
-- -- -- foldM
