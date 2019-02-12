module Account where 

-- import Prelude hiding (lookup)     
import Data.Map (fromList, adjust, Map, member)
import qualified Data.Map (map)

data Category = Asset | Expense | Equity | Liability | Income | Profit deriving (Show, Eq)
data Side = Debit | Credit
-- aльтернатива - добавиьт конструктор над категориями https://pastebin.com/kpPD3XWU
sumSide :: Category -> Side
sumSide category = if category `elem` [Asset, Expense] then Debit else Credit
type Amount = Float
data Account = Account Category [Amount] [Amount] deriving (Show, Eq)
type AccountName = String
type AccountMap = Map AccountName Account
type SafeAccountMap = Either String AccountMap 

nullAccount :: Category -> Account  
nullAccount category = Account category [] []    

-- упрощение просмотра, сверунять список операций 
account (Account t debits credits) = Account t [sum debits] [sum credits]

-- функции для изменения либо правой, либо левой стороны счета 
debit :: Amount -> Account -> Account
debit x (Account t debits credits) = Account t (x:debits) credits 
credit :: Amount -> Account -> Account
credit x (Account t debits credits) = Account t debits (x:credits)

-- рассчитываем баланс на стороне дебита или кредита 
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

unpackTuple :: (String, [String]) -> [(AccountName, Category)]
unpackTuple (categoryStr, names) = case readCategory categoryStr of
    Just category -> zip names (repeat category)
    Nothing -> []

asAccountMap :: [(AccountName, Category)] -> SafeAccountMap
asAccountMap tuples = Right $ fromList (map f' tuples)
    where f' (name, cat) = (name, nullAccount cat)      

-- получаем иницилизированную систему пустых счетов, 
-- где назания счетов - ключи словаря AccountMap
makeAccountMap :: [(String, [String])] -> SafeAccountMap
makeAccountMap source = asAccountMap (concatMap unpackTuple source)

-- управление SafeAccountMap  
changeByKeyWith :: AccountName -> (Account -> Account) -> SafeAccountMap -> SafeAccountMap  
changeByKeyWith key action (Right accounts) = if member key accounts 
    then Right $ adjust action key accounts -- maybe use `update`?
    else Left $ "Account name not found: " ++ key
changeByKeyWith _ _ (Left message) = Left message

debitByKey name amount  = changeByKeyWith name (debit amount)  
creditByKey name amount = changeByKeyWith name (credit amount)  

data Entry = Entry AccountName AccountName Amount
applyEntry :: SafeAccountMap -> Entry -> SafeAccountMap
-- надо ли перейти к do-нотации?
applyEntry accountsM (Entry key1 key2 amount) = debitByKey key1 amount accountsM'
    where accountsM' = creditByKey key2 amount accountsM 
-- shorthand version
mut = applyEntry 

-- "Вьюхи"
balances :: SafeAccountMap -> Either String (Map AccountName Amount)
balances = fmap (Data.Map.map balance)
