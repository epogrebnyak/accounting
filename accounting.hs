-- Minimal double-entry accounting system

import qualified Data.Map.Internal as Map
import Data.Map (fromList, toList, alter, lookup)

type Amount = Float
data Category = Asset | Expense | Equity | Liability | Income | Profit deriving Show

data Account = Account {
    category :: Category, 
    debits :: [Amount], 
    credits :: [Amount]
    } deriving (Show)
data Side = Debit | Credit

type OperationName = String
type AccountName = String
-- корреспонденция счетов для проводки
data Pair = Pair { 
    debitAccount :: AccountName, 
    creditAccount :: AccountName
    }
-- словари    
type Accounts = Map.Map AccountName Account
type OperationCatalog = Map.Map OperationName Pair

-- типы для наглядного ввода данных в тестах и примерах
data Entry = Entry AccountName AccountName Amount
data Operation = Operation OperationName Amount

nullAccount :: Category -> Account  
nullAccount category = Account category [] []

-- баланс по правой или левой стороне счета в зависимости от 
-- категории счета  
sumSide Asset = Debit
sumSide Expense = Debit
sumSide _ = Credit

balance :: Account -> Amount
balance (Account cat debits credits) = case sumSide cat of
    Debit -> s
    Credit -> -s 
    where s = sum(debits) - sum(credits) 

-- функции для увеличения либо правой, либо левой стороны счета 
append :: [a] -> a -> [a]
append xs x = xs ++ [x]
debit :: Amount -> Account -> Account
debit x (Account t debits credits) = Account t (append debits x) credits 
credit :: Amount -> Account -> Account
credit x (Account t debits credits) = Account t debits (append credits x)

-- план счетов и каталог проводок
fromListWith :: (a -> (String, b)) -> [a] -> Map.Map String b
fromListWith f x = fromList (map f x)

readCategory "asset" = Asset
readCategory "equity" = Equity
readCategory "liability" = Liability
readCategory "expense" = Expense
readCategory "income" = Income
readCategory _ = error "Wrong category"

makeAccountMap :: [(String, String)] -> Accounts
makeAccountMap = fromListWith f 
    where f (cat, accName) = (accName::AccountName,  nullAccount (readCategory cat)) 

makeOperationsMap :: [(String, String, String)] -> OperationCatalog
makeOperationsMap = fromListWith f
    where f (a, b, c)  = (a::OperationName, Pair (b::AccountName) (c::AccountName))   

getCorrespondence :: OperationCatalog -> OperationName -> Pair 
getCorrespondence catalog name = case Data.Map.lookup name catalog of
    Just pair -> pair
    Nothing -> error "Wrong operation"   

-- проведение проводки 
mutate :: Accounts -> Pair -> Amount -> Accounts
mutate accounts (Pair debitName creditName) amount = 
    сhangeWith creditName credit' $ сhangeWith debitName debit' accounts
    where 
        debit' (Just account) = Just (debit amount account)  
        debit' Nothing = error $ "Wrong debit account name: " ++ debitName
        credit' (Just account) = Just (credit amount account)
        credit' Nothing = error $ "Wrong credit account name: " ++ creditName
        -- alter f key m, where f is function, and m is map     
        сhangeWith key f account = alter f key accounts

-- сахар для быстрого изменения счетов         
mut :: Accounts -> Entry -> Accounts
mut account (Entry dacc cacc amount) = mutate accounts (Pair dacc cacc) amount

-- проведелние операций 
runOne :: OperationCatalog -> Accounts -> Operation -> Accounts
runOne catalog accounts (Operation name amount) = mutate accounts pair amount
    where pair = getCorrespondence catalog name
runMany :: OperationCatalog -> Accounts -> [Operation] -> Accounts
runMany catalog accounts operations = foldl (runOne catalog) accounts operations

-- те ли названия счетов использует каталог операций?
isCompatible :: OperationCatalog -> Accounts -> Bool
isCompatible catalog operations = error "Not implemented"

-- названия счетов (пример)
accountNames = [ 
    ("asset", "cash"),
    ("equity", "capital"),
    ("expense", "interest"),
    ("liability", "debt")
    ]

-- каталог проводок (пример)
allowedEntries = [
    ("add capital", "cash", "equity"),
    ("borrow", "cash", "debt"),
    ("accrue interest", "interest", "debt"),
    ("pay debt", "debt", "cash")
    ]

-- Примеры    
a = Account Asset [] []
z = balance $ credit 7 (debit 10 a) -- 3

-- читаем план счетов и каталог проводок
accounts = makeAccountMap accountNames 
opCatalog = makeOperationsMap allowedEntries

-- пробуем "низкоуровневые" проводки
e1 = Entry "cash" "capital" 5
e2 = Entry "cash" "debt" 2
accounts1 = mut (mut accounts e1) e2

-- на этих же счетах делаем операцию
processOp = runOne opCatalog
accounts2 = processOp accounts1 (Operation "pay debt" 2)
-- accounts2' = написать выражение для проверки 

-- проводим серию операций на чистом балансе 
processMany = runMany opCatalog
acc' = processMany accounts [Operation "add capital" 100, 
        Operation "borrow" 400, 
        Operation "accrue interest" 40,
        Operation "pay debt" 440]

-- здесь демонстрируется ошибка (задание, какая, где?)        
acc1 = runOne opCatalog accounts (Operation "add capital" 100)
acc1' = mut accounts $ Entry "cash" "capital" 100

-- Далее:
-- проверка совместимости operation и assets
-- юнит тесты 
-- проверка равенства для счета deriving Eq
-- "вьюха" компрессия в баланс
-- "вьюха" компрессия в p&l