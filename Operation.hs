-- Minimal double-entry accounting system
import Prelude hiding (lookup, Map)   
import Data.Map (fromList, toList, alter, lookup, Map)
import Account (Pair(..), Amount, AccountName, AccountMap, mutate)
import Account (accountsExample)

type OperationName = String 
type OperationCatalog = Map OperationName Pair
data Operation = Operation OperationName Amount

fromListWith :: (a -> (String, b)) -> [a] -> Map String b
fromListWith f x = fromList (map f x)

makeOperationsMap :: [(String, String, String)] -> OperationCatalog
makeOperationsMap = fromListWith f
    where f (a, b, c)  = (a::OperationName, Pair (b::AccountName) (c::AccountName))   

getCorrespondence :: OperationCatalog -> OperationName -> Pair 
getCorrespondence catalog name = case lookup name catalog of
    Just pair -> pair
    Nothing -> error "Wrong operation"   

-- проведелние операций 
runOne :: OperationCatalog -> AccountMap -> Operation -> AccountMap
runOne catalog accounts (Operation name amount) = mutate accounts pair amount
    where pair = getCorrespondence catalog name
runMany :: OperationCatalog -> AccountMap -> [Operation] -> AccountMap
runMany catalog accounts operations = foldl (runOne catalog) accounts operations

-- каталог проводок (пример)
allowedEntries = [
    ("add capital", "cash", "equity"),
    ("borrow", "cash", "debt"),
    ("accrue interest", "interest", "debt"),
    ("pay debt", "debt", "cash")
    ]


-- читаем каталог проводок
opCatalog = makeOperationsMap allowedEntries

-- на этих же счетах делаем операцию
processOp = runOne opCatalog
accounts2 = processOp accountsExample (Operation "pay debt" 2)

-- проводим серию операций на чистом балансе 
processMany = runMany opCatalog
acc' = processMany accountsExample [Operation "add capital" 100, 
        Operation "borrow" 400, 
        Operation "accrue interest" 40,
        Operation "pay debt" 440]

-- здесь демонстрируется ошибка в названиях счетов - (задание, какая, где?)        
acc1 = runOne opCatalog accountsExample (Operation "add capital" 100)

-- Ошибка: почему-то не импортируется конструтор Pair
-- acc1' = mutate accounts (Pair "cash" "capital") 100

-- Далее:
-- проверка совместимости operation и assets
-- юнит тесты 
-- проверка равенства для счета deriving Eq
-- "вьюха" баланса
-- "вьюха" p&l

-- те ли названия счетов использует каталог операций?
isCompatible :: OperationCatalog -> AccountMap -> Bool
isCompatible catalog operations = error "Not implemented"
