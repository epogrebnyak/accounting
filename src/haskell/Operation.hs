-- Minimal double-entry accounting system
module Operation where 

import Prelude hiding (lookup, Map)   
import Data.Map (fromList, lookup, Map)
import Account (Amount, AccountName, SafeAccountMap, Entry(..), mut)

type OperationName = String 
type OperationCatalog = Map OperationName (AccountName, AccountName)
data Operation = Operation OperationName Amount

-- читаем каталог опeрций 
makeOperationsMap :: [(String, String, String)] -> OperationCatalog
makeOperationsMap x = fromList $ map f x
    where f (a, b, c)  = (a::OperationName, (b::AccountName, c::AccountName))   

-- проведение операций 
runOne :: OperationCatalog -> SafeAccountMap -> Operation -> SafeAccountMap
runOne catalog accounts (Operation name amount) = case lookup name catalog of
    Just (acc1, acc2) -> mut accounts $ Entry acc1 acc2 amount
    Nothing -> Left $ "Operation not found " ++ name

runMany :: OperationCatalog -> SafeAccountMap -> [Operation] -> SafeAccountMap
runMany catalog  = foldl (runOne catalog) 