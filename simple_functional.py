
ACCOUNTS =  ['cash', 'inventory', 'cost of goods sold', 
             'income', 'capital']        

def init_accounts():
    return {acc:0 for acc in ACCOUNTS}

def move(accounts, x, from_acc, to_acc):
    accounts[from_acc] -= x
    accounts[to_acc] +=  x
    return accounts
    
def increase(accounts, x, acc1, acc2):
    accounts[acc1] += x
    accounts[acc2] += x
    return accounts

def decrease(accounts, x, acc1, acc2):
    increase(accounts, -1 * x, acc1, acc2)
    return accounts

OPERATIONS = {"pay in capital": (increase, "cash", "capital"),
              "buy goods": (move, "cash", "inventory"),
              "ship goods": (move, "inventory", "cost of goods sold"),
              "get payment": (increase, "cash", "income")
              }
    
def process(accounts, transaction):
    action_name, amount = transaction
    (f, acc1, acc2) = OPERATIONS[action_name]
    return f(accounts, amount, acc1, acc2)
        
def profit(accounts):
    return accounts['income'] - accounts['cost of goods sold']  
        
accounts = init_accounts()
ledger = [("pay in capital", 100), 
          ("buy goods", 75), 
          ("ship goods", 50),
          ("get payment", 60),
          #("buy goods", 35), 
          #("sell goods", 40), 
           ]

for transaction in ledger: 
    accounts = process(accounts, transaction)
    
print (accounts) 
print (profit(accounts))    
        
    
