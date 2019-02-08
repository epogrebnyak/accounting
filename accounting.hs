-- Minimal double-entry accounting system

import qualified Data.Map.Internal as Map
import Data.Map (fromList, toList, alter, lookup)

type Amount = Float
data Category = Asset | Expense | Equity | Liability | Income | Profit deriving Show

data Account = Account Category [Amount] [Amount] deriving Show
data Side = Debit | Credit

type OperationName = String
type AccountName = String
data Ref = Ref AccountName | NoRef deriving Show
type AccountMap = Map.Map AccountName Account
type OperationMap = Map.Map OperationName (Ref, Ref)

nullAccount :: Category -> Account  
nullAccount cat = Account cat [] []

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
fromListWith :: (a -> (String, b)) -> [a] -> Map.Map String b
fromListWith f x = fromList (map f x)

makeAccountMap :: [(Category, String)] -> AccountMap
makeAccountMap = fromListWith f 
    where f (cat, name) = (name::AccountName,  nullAccount cat) 

makeOperationsMap :: [(String, String, String)] -> OperationMap
makeOperationsMap = fromListWith f
    where f (a, b, c)  = (a::OperationName, (Ref b, Ref c)) 
    
-- проводка     
data Entry = Entry {
    debitAccount :: Ref, 
    creditAccount :: Ref, 
    amount :: Amount
    }


mutate accounts (Entry (Ref debitAccount) (Ref creditAccount) amount) = 
    -- alter f key m, where f is function, and m is map 
    alter credit' creditAccount $ alter debit' debitAccount accounts
    where 
        debit' (Just account) = Just (debit amount account)  
        debit' Nothing = Nothing 
        credit' (Just account) = Just (credit amount account)
        credit' Nothing = Nothing


getEntry :: OperationMap -> OperationName -> Amount -> Entry 
getEntry operationMap op amount = case Data.Map.lookup op operationMap of
        Just (r1, r2) -> Entry r1 r2 amount
        Nothing -> error "Wrong operation"   


runOperation :: OperationMap -> AccountMap -> OperationName -> Amount -> AccountMap
runOperation operationMap accountMap op amount = mutate accountMap _e
    where _e = getEntry operationMap op amount

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
opCatalog = makeOperationsMap allowedEntries
process = runOperation opCatalog
accounts4 = process accounts'' "pay debt" 0.2

-- Далее:
-- process a list of operations - maybe a fold! 