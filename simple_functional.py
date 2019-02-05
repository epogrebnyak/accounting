"""Minimal double-entry accounting example with 
   T-accounts, legder and transaction catalog.
"""

from tabulate import tabulate
ACCOUNT_NAMES = dict(asset=['cash', 'inventory'], 
                     equity=['capital'], 
                     income=['sales'], 
                     expense=['cogs'])
OPERATIONS = {"pay in capital": ("cash", "capital"),
              "buy goods": ("inventory", "cash"),
              "ship goods": ("cogs", "inventory"),
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
        return -1 * self._debit_balance()   

class BalanceSheet:
    pass

class Asset(DebitAccount, BalanceSheet):
    pass
        
class Equity(CreditAccount, BalanceSheet):
    pass

class Liability(CreditAccount, BalanceSheet):
    pass
        
class Expense(DebitAccount):
    pass

class Income(CreditAccount):
    pass

class Profit(Equity):
    pass

CLASS_CONSTRUCTORS = dict(asset=Asset,
                          equity=Equity,
                          liability=Liability,
                          expense=Expense,
                          income=Income,
                          profit=Profit)

def make_instance(keyword, account_name):
    return CLASS_CONSTRUCTORS[keyword](account_name)


def init_accounts(account_names=ACCOUNT_NAMES):
    return {v:make_instance(kw, v) 
    for kw, values in account_names.items() for v in values} 
    
def mutate(accounts, debit_side, credit_side, amount):
    accounts[debit_side].debit(amount)
    accounts[credit_side].credit(amount)
    return accounts

def process(accounts, event, catalog=OPERATIONS):
    action_name, amount = event
    acc1, acc2 = catalog[action_name]
    return mutate(accounts, acc1, acc2 , amount)
       
# Reference functions 

def profit(accounts):
    return account_sum(accounts, Income) - account_sum(accounts, Expense)

def account_list(accounts):
    return accounts.values()

def account_list_by_type(accounts, cls):
    return [a for a in accounts.values() if isinstance(a, cls)]

def account_dict_by_type(accounts, cls):
    return {k:a for (k,a) in accounts.items() if isinstance(a, cls)}

def total(accounts_list):
    return sum(map(lambda x: x.balance, accounts_list))

def account_sum(accounts, cls):
    return total(account_list_by_type(accounts, cls))

def has_identity(accounts):    
    return account_sum(accounts, DebitAccount) == \
           account_sum(accounts, CreditAccount)

def values_dict(accounts):
    return {k: v.balance for k,v in accounts.items()}

def balance_sheet(accounts):
    _accounts = accounts.copy()
    p = Profit('profit', profit(_accounts))
    _accounts['profit'] = p
    return account_dict_by_type(_accounts, BalanceSheet)

def balance_dict(accounts):
    b = balance_sheet(accounts)
    mk = lambda cls: values_dict(account_dict_by_type(b, cls))
    return dict(assets=mk(Asset), 
                equity=mk(Equity),
                liabilities=mk(Liability))

def balance_table_items(accounts):
    bd = balance_dict(accounts)    
    for key in bd.keys():
        yield key.capitalize(), None
        for k,v in bd[key].items():
            yield "- "+k.capitalize(), v

def print_balance(accounts):
    from tabulate import tabulate
    text = tabulate(balance_table_items(accounts), ("",""), "plain")
    print(text)
                              


if '__main__' == __name__:            
    accounts = init_accounts()
    events = [("pay in capital", 100), 
              ("buy goods", 75), 
              ("ship goods", 50),
              ("get payment", 60)
               ]    
    for event in events: 
        process(accounts, event)
        assert has_identity(accounts)         
    print(balance_dict(accounts)) 
    print_balance(accounts)    
    assert values_dict(accounts) == {'capital': 100, 'cash': 85, 'cogs': 50, 'inventory': 25, 'sales': 60}
    assert profit(accounts) == 10
    assert has_identity(accounts)