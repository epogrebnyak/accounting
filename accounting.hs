-- Minimal double-entry accounting system

import Data.Map (fromList, alter)

data Type = Asset | Expense | Equity | Liability | Income | Profit deriving Show
data Account = Account Type [Float] [Float] deriving Show

-- функции для увеличения либо правой либо левой стороны счета 
append xs x = xs ++ [x]
debit x (Account t debits credits) = Account t (append debits x) credits 
credit x (Account t debits credits) = Account t debits (append credits x)

-- Нам нужно сгруппировать конструктору из Type в две группы, так чтобы 
-- функция balance в одной группы выдавала выражение (debits - credits),
-- а в другой группе (credits-debits).

-- Сейчас это достигается системой проверок is_debit. Можно ли 
-- свернуть в более элегантную реализацию? Идея из других языков:
-- наследовать "типы"  
-- Account -> DebitAccount | CrediAccount
-- DebitAccount -> Asset | Expense
-- CrediAccount -> Equity | Liability | Income | Profit
-- и далее balance могла бы вести себя по-разному в зависимости 
-- от типа DebitAccount или DebitAccount
is_debit Asset = True
is_debit Expense = True
is_debit _ = False
balance (Account t debits credits) = s * (if is_debit t then 1 else -1)   
    where s = sum(debits) - sum(credits)  

-- Возможно мне нужно что-то из конструкций ниже 
-- но тут мне не хватает ума понять, как будут введены конкретные 
-- типы Asset | Expense | Equity | Liability | Income | Profit
type Amount = Float
class TAccount a where
    debit :: Amount -> a -> a   
    credit :: Amount -> a -> a 
    balance :: a -> Amount
instance TAccount DebitAccount where
    -- здесь balance делает что-то одно 
    balance :: a -> Amount
instance TAccount CreditAccount where
    -- здесь balance делает что-то другое 
    balance :: a -> Amount


-- Далее в целом работет, текущих вопросов нет.
makeAccounts accNames = fromList $ map makeAccount accountNames  
    where makeAccount (t, name) = (name, Account t [] [])
debit' x (Just acc) = Just (debit x acc)  
credit' x (Just acc) = Just (credit x acc)

data Entry = Entry {
    debitAccount :: String, 
    creditAccount :: String, 
    amount :: Float
    } 

processEntry accounts (Entry da ca x) = alter (credit' x) ca $ alter (debit' x) da accounts

accountNames = [ 
    (Asset, "cash"),
    (Equity, "capital")
    ]

-- Пробуем тут     
a = Account Asset [] []
z = balance $ credit 7 (debit 10 a) -- 3
accounts = makeAccounts accountNames 
e = Entry "cash" "capital" 5
accounts' = processEntry accounts e