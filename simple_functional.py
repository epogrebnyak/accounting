"""Minimal double-entry accounting example with 
   T-accounts, legder and transaction catalog.
"""
DEBIT_ACCOUNTS = ['cash', 'inventory', 'cost of goods sold'] 
CREDIT_ACCOUNTS = ['capital', 'sales']
OPERATIONS = {"pay in capital": ("cash", "capital"),
              "buy goods": ("inventory", "cash"),
              "ship goods": ("cost of goods sold", "inventory"),
              "get payment": ("cash", "sales")
              }   

class Account:
    def __init__(self, name, start_balance=0):
        self.name = name
        self.debits = [start_balance]
        self.credits = []

    def debit(self, x):
        self.debits.append(x)

    def credit(self, x):
        self.credits.append(x)

    def _debit_balance(self):
        return sum(self.debits) - sum(self.credits)

    @property
    def balance(self):
        return self._debit_balance()       
   
    def __repr__(self):
        return "%s('%s', %.1f)" % (self.__class__.__name__,
                                   self.name,
                                   float(self.balance))    
    
    def __eq__(self, x):
        return (self.__class__ == x.__class__) & (self.balance == x.balance)        
    
class DebitAccount(Account):
    pass

class CreditAccount(Account):
    def __init__(self, name, start_balance=0):
        self.name = name
        self.debits = []
        self.credits = [start_balance]
        
    @property
    def balance(self):
        return -self._debit_balance()   

def init_accounts(debit_names=DEBIT_ACCOUNTS, 
                  credit_names=CREDIT_ACCOUNTS):
    ac1 = [DebitAccount(name) for name in debit_names]                  
    ac2 = [CreditAccount(name) for name in credit_names]                  
    return {a.name:a for a in ac1+ac2}

def mutate(accounts, debit_side, credit_side, amount):
    accounts[debit_side].debit(amount)
    accounts[credit_side].credit(amount)
    return accounts

def process(accounts, event, catalog=OPERATIONS):
    action_name, amount = event
    acc1, acc2 = catalog[action_name]
    return mutate(accounts, acc1, acc2 , amount)

class Company:
    def __init__(self, name, accounts=None, transaction_catalog=OPERATIONS):
        self.name = name
        if accounts is None:
            self.dict = init_accounts()
        else:    
            self.accounts = accounts
        self.catalog = transaction_catalog
        
    def record(self, action_name, amount):
        try:
            acc1, acc2 = self.catalog[action_name]
            self.dict = mutate(self.dict, acc1, acc2, amount)
        except KeyError:
            raise ValueError('Unknown operation: ' + action_name) 
            
       
# reference functions       
        
def profit(accounts):
    return accounts['sales'].balance - accounts['cost of goods sold'].balance  

def list_accounts_by_type(accounts, cls):
    return [a for a in accounts.values() if isinstance(a, cls)]

def debit_accounts(accounts):
    return list_accounts_by_type(accounts, cls=DebitAccount)

def credit_accounts(accounts):
    return list_accounts_by_type(accounts, cls=CreditAccount)

def total(accounts_list):
    return sum(map(lambda x: x.balance, accounts_list))

def gross_asset_total(accounts):
    return total(debit_accounts(accounts))
    
def gross_el_total(accounts):
    return total(credit_accounts(accounts))

def values(accounts):
    return {k:v.balance for k,v in accounts.items()}

if '__main__' == __name__:            
    accounts = init_accounts()
    events = [("pay in capital", 100), 
              ("buy goods", 75), 
              ("ship goods", 50),
              ("get payment", 60)
               ]
    
    for event in events: 
        process(accounts, event)
        assert gross_asset_total(accounts) == gross_el_total(accounts)
        
    print(accounts) 
    
    assert values(accounts) == {'capital': 100,
     'cash': 85,
     'cost of goods sold': 50,
     'inventory': 25,
     'sales': 60}
    
    assert profit(accounts) == 10
    assert gross_asset_total(accounts) == gross_el_total(accounts)
    
    
    r = Company("ABC")
    for event in events: 
        r.record(*event)
    print(r.dict)

    assert r.dict == accounts   

