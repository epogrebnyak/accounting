data Category = Asset | Expense | Equity | Liability | Income | Profit
type Amount = Float
data Account = Account Category [Amount] [Amount] deriving (Show, Eq)

total xs ys = sum(xs) - sum(ys)  
balance Account (Debit _) debits credits = total debits credits
balance Account (Credit _) debits credits = - total debits credits
account Account (SumDebit cat) debits credits = Account (SumDebit cat) 

debitSide Asset = True
debitSide Expense = True
debitSide _ = False