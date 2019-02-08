-- Minimal double-entry accounting system


import qualified Data.Map.Internal
import Data.Map (fromList, toList, alter, lookup)

type Amount = Float
data Category = Asset | Expense | Equity | Liability | Income | Profit deriving Show
type Name = String
data Account = Account Category [Amount] [Amount] deriving Show
data Side = Debit | Credit
data Ref = Ref Name deriving Show

type AccountMap = Data.Map.Internal.Map Name Account
type OperationMap = Data.Map.Internal.Map Name (Ref, Ref)

nullAccount :: Category -> Account  
nullAccount t = Account t [] []

category :: Account -> Category
category (Account cat debits credits) = cat

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

-- план счетов 
fromListWith :: (a -> (String, b)) -> [a] -> Data.Map.Internal.Map String b
fromListWith f x = fromList (map f x)

makeAccountMap :: [(Category, String)] -> AccountMap
makeAccountMap = fromListWith f 
    where f (t, name) = (name::Name,  nullAccount t) 

-- makeOperationsMap :: [(String, String, String)] -> AccountMap
makeOperationsMap :: [(String, String, String)] -> OperationMap
makeOperationsMap = fromListWith f
    where f (a, b, c)  = (a::Name, (Ref b, Ref c)) 
    
-- проводка     
data Entry = Entry {
    debitAccount :: Ref, 
    creditAccount :: Ref, 
    amount :: Amount
    }

process :: AccountMap -> Entry -> AccountMap
mutate accounts (Entry (Ref debitAccount) (Ref creditAccount) amount) = 
    -- alter f key m, where f is function, and m is map 
    alter credit' creditAccount $ alter debit' debitAccount accounts
    where 
        debit' (Just account) = Just (debit amount account)  
        debit' Nothing = Nothing 
        credit' (Just account) = Just (credit amount account)
        credit' Nothing = Nothing

getEntry :: OperationMap -> Name -> Amount -> Entry 
getEntry operationMap op amount = case Data.Map.lookup op operationMap of
        Just (r1, r2) -> Entry r1 r2 amount
        Nothing -> error "Wrong operation"   

makeOperation :: OperationMap -> AccountMap -> Name -> Amount -> AccountMap
makeOperation operationMap accountMap op amount = mutate accountMap e1
    where e = getEntry operationMap op amount

-- операции

allowedEntries = [
    ("borrow", "cash", "debt"),
    ("accrue interest", "interest", "debt"),
    ("pay debt", "debt", "cash")
    ]

accountNames = [ 
    (Asset, "cash"),
    (Equity, "capital"),
    (Expense, "interest"),
    (Liability, "debt")
    ]


-- Примеры    
a = Account Asset [] []
z = balance $ credit 7 (debit 10 a) -- 3
accounts = makeAccountMap accountNames 
e1 = Entry (Ref "cash") (Ref "capital") 5
accounts' = mutate accounts e1
e2 = Entry (Ref "cash") (Ref "debt") 2
accounts'' = mutate accounts' e2
ops = makeOperationsMap allowedEntries
process = makeOperation ops 
accounts4 = makeOperation ops accounts'' "pay debt" 2

-- Далее
-- укоротить импорты 
-- Name -> AccountName OperationName
