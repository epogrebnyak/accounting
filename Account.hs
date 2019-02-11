module Account where 

import Prelude hiding (lookup, Map)     
import Data.Map (fromList, toList, alter, lookup, Map, map)

data Category = Asset | Expense | Equity | Liability | Income | Profit deriving (Show, Eq)
type Amount = Float
data Account = Account Category [Amount] [Amount] deriving (Show, Eq)
type AccountName = String
type AccountMap = Map AccountName Account

nullAccount :: Category -> Account  
nullAccount category = Account category [] []    

-- задача 1: прочитать из accountNames словарь типа AccountMap
--           accountNames может приходит как JSON
--           В результате мы получаем иницилизированную систему пустых счетов, 
--           где назания счетов - ключи словаря AccountMap
readCategory "asset" = Asset
readCategory "equity" = Equity
readCategory "liability" = Liability
readCategory "expense" = Expense
readCategory "income" = Income
readCategory c = error ("Wrong category: " ++ c)

readAccountsByName :: [(String, [String])] -> AccountMap
readAccountsByName accnames = fromList $ concatMap (uncurry f) accnames
    where f category names = zip names $ repeat (nullAccount (readCategory category))

accountNames = [ 
    ("asset", ["cash", "receivable", "inventory"]),
    ("equity", ["capital"]),
    ("expense", ["cogs", "interest"]),
    ("liability", ["payable", "debt"])
    ]

accountsExample = readAccountsByName accountNames

-- задача 2: ввести функции для изменения либо правой, либо левой стороны счета 
append :: [a] -> a -> [a]
append xs x = xs ++ [x]
debit :: Amount -> Account -> Account
debit x (Account t debits credits) = Account t (append debits x) credits 
credit :: Amount -> Account -> Account
credit x (Account t debits credits) = Account t debits (append credits x)

-- задача 3: считать баланс по правой или левой стороне счета в зависимости 
-- от категории счета  
-- фактически эти привязка двух категорий типа Category к категории Side 
data Side = Debit | Credit
sumSide Asset = Debit
sumSide Expense = Debit
sumSide _ = Credit

balance :: Account -> Amount
balance (Account cat debits credits) = case sumSide cat of
    Debit -> s
    Credit -> -s 
    where s = sum(debits) - sum(credits) 

-- задача 4: уметь делать проводки - менять счета внутри AccountMap
data Pair = Pair { 
    debitAccount :: AccountName, 
    creditAccount :: AccountName
    }

mutate :: AccountMap -> Pair -> Amount -> AccountMap
mutate accounts (Pair debitName creditName) amount = 
    -- alter f key m, where f is function, and m is map   
    alter credit' creditName $ alter debit' debitName accounts
    where 
        debit' (Just account) = Just (debit amount account)  
        debit' Nothing = error $ "Wrong debit account name: " ++ debitName
        credit' (Just account) = Just (credit amount account)
        credit' Nothing = error $ "Wrong credit account name: " ++ creditName

data Entry = Entry AccountName AccountName Amount
mut accounts (Entry da ca amount) = mutate accounts (Pair da ca) amount


-- Примеры    
a = Account Asset [] []
z = balance $ credit 7 (debit 10 a) -- 3

e1 = Entry "cash" "capital" 5
e2 = Entry "cash" "debt" 2
-- accountsExample определен выше 
accounts1 = mut (mut accountsExample e1) e2


accounts' = fromList [("cash", nullAccount Asset), ("capital",nullAccount Equity)]
b = foldl mut accounts' [(Entry "cash" "capital" 100), -- взнос акционеров в капитал
                         (Entry "cash" "capital" 20),  -- взнос акционеров в капитал
                         (Entry "capital" "cash" 70)   -- выплата акционерам
                        ] 
c = fromList [("capital",Account Equity [] [50]),
              ("cash",Account Asset [50] [])]


-- Data.Map.map balance b == Data.Map.map balance c
-- True

