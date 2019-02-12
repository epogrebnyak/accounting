-- our API
import Account (makeAccountMap, Entry(..), mut)
import Operation (makeOperationsMap, runOne, runMany, Operation(..))

-- for testing 
import Data.Map (fromList)
import Account (Account(..), Category(..), nullAccount, debitByKey, 
                creditByKey, balances, asAccountMap)

accountNames = [ 
        ("asset", ["cash", "receivable", "inventory"]),
        ("equity", ["capital"]),
        ("expense", ["cogs", "interest"]),
        ("liability", ["payable", "debt"])
    ]

allowedEntries = [
        ("add capital", "cash", "capital"),
        ("borrow", "cash", "debt"),
        ("accrue interest", "interest", "debt"),
        ("pay debt", "debt", "cash")
        ]

accounts = makeAccountMap accountNames
catalog = makeOperationsMap allowedEntries
-- короткие функции для обработки операций 
fit = runOne catalog
fits = runMany catalog

main = do 
    putStrLn "Empty accounts:"
    putStrLn $ show accounts
    putStrLn "Modified by <add capital 10>:"
    let accounts' = show $ fit accounts (Operation "add capital" 10) 
    putStrLn $ accounts'

-- 0. действия над счетами - в тестах

-- 1. проводки
n = asAccountMap [("cash", Asset),("paid-in capital", Equity)]
w = mut n $ Entry "cash" "paid-in capital" 10    
       
-- делаем две проводки 
e1 = Entry "cash" "capital" 100
e2 = Entry "cash" "debt" 400
accounts1 = mut (mut accounts e1) e2
accounts1' = foldl mut accounts [e1, e2]
chk = accounts1 == accounts1' -- True

-- делаем три проводки
b' = foldl mut n [(Entry "cash" "capital" 100), -- взнос акционеров в капитал
                  (Entry "cash" "capital" 20),  -- взнос акционеров в капитал
                  (Entry "capital" "cash" 70)   -- выплата акционерам
                  ] 
c' = Right $ fromList [("capital",Account Equity [] [50]),
                       ("cash",Account Asset [50] [])]
-- сравниваем результат 
k = (balances b' == balances c') -- True

-- 2. операции 
-- на этих же счетах делаем одну операцию
acc' = fit accounts (Operation "add capital" 100) 

-- проводим серию операций на чистом балансе 
acc'' = fits accounts [Operation "add capital" 100, 
        Operation "borrow" 400, 
        Operation "accrue interest" 40,
        Operation "pay debt" 440]
