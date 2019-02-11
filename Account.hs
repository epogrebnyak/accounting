module Account where 

import Prelude hiding (lookup, map)     
import Data.Map (fromList, toList, adjust, lookup, Map, map, member)

data Category = Asset | Expense | Equity | Liability | Income | Profit deriving (Show, Eq)
data Side = Debit | Credit
-- aльтернатива - https://pastebin.com/kpPD3XWU
sumSide :: Category -> Side
sumSide category = if category `elem` [Asset, Expense] then Debit else Credit
type Amount = Float
data Account = Account Category [Amount] [Amount] deriving (Show, Eq)
type AccountName = String
type AccountMap = Map AccountName Account
type SafeAccountMap = Either String AccountMap 

nullAccount :: Category -> Account  
nullAccount category = Account category [] []    

-- упрощение просмотра 
account (Account t debits credits) = Account t [sum debits] [sum credits]

-- функции для изменения либо правой, либо левой стороны счета 
debit :: Amount -> Account -> Account
debit x (Account t debits credits) = Account t (x:debits) credits 
credit :: Amount -> Account -> Account
credit x (Account t debits credits) = Account t debits (x:credits)

-- рассчитываем баланс на стороне дебита ил кредита 
balance :: Account -> Amount
balance (Account category debits credits) = case sumSide category of
    Debit -> s
    Credit -> -s 
    where s = sum(debits) - sum(credits) 

readCategory:: String -> Maybe Category
readCategory "asset" = Just Asset 
readCategory "equity" = Just Equity
readCategory "liability" = Just Liability
readCategory "expense" = Just Expense
readCategory "income" = Just Income
readCategory _ = Nothing

unpackTuple :: (String, [String]) -> [(AccountName, Account)]
unpackTuple (categoryStr, names) = case readCategory categoryStr of
    Just category -> zip names (repeat (nullAccount category))
    Nothing -> []

-- получаем иницилизированную систему пустых счетов, 
-- где назания счетов - ключи словаря AccountMap
unpackTuples :: [(String, [String])] -> SafeAccountMap
unpackTuples source = Right $ fromList $ concatMap unpackTuple source

   
changeByKeyWith :: AccountName -> (Account -> Account) -> SafeAccountMap -> SafeAccountMap  
changeByKeyWith key action (Right accounts) = if member key accounts 
    then Right $ adjust action key accounts -- maybe use `update`?
    else Left $ "Account name not found: " ++ key
changeByKeyWith _ _ (Left message) = Left message

debitByKey name amount  = changeByKeyWith name (debit amount)  
creditByKey name amount = changeByKeyWith name (credit amount)  

data Entry = Entry AccountName AccountName Amount
applyEntry :: SafeAccountMap -> Entry -> SafeAccountMap
applyEntry accountsM (Entry key1 key2 amount) = f $ g accountsM
    where 
        f = debitByKey key1 amount  
        g = creditByKey key2 amount 
applyEntries = foldl applyEntry

-- shorthand notation 
mut = applyEntry 
muts = applyEntries 

-- -- в Quickcheck
balances :: SafeAccountMap -> Either String (Map AccountName Amount)
balances = fmap (Data.Map.map balance)


-- -- Примеры    
accountNames = [ 
        ("asset", ["cash", "receivable", "inventory"]),
        ("equity", ["capital"]),
        ("expense", ["cogs", "interest"]),
        ("liability", ["payable", "debt"])
        ]
accountsExample = unpackTuples accountNames


n = Right $ fromList([("cash", nullAccount Asset),("paid-in capital", nullAccount Equity)])
w = applyEntry n $ Entry "cash" "paid-in capital" 10    

s = Right $ fromList [("a", nullAccount Asset)]
d = debitByKey "a" 1  s
-- d
-- Right (fromList [("a",2)])
d' = creditByKey "q" 1 s
         
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

-- balances b' == balances c'
-- True

-- -- Data.Tree
-- -- POSTGRES
-- -- sqlite-simple
-- -- Persist* / Yesod
-- -- Aeson
-- -- smart конструктор
-- -- -- foldM
