"""Minimal double-entry accounting example with 
   T-accounts, legder and accounting entries catalog.
"""
import collections
import itertools 

class Account:
    def __init__(self, name, start_balance=0):
        self.name = name
        self.debits = [start_balance]
        self.credits = []

    def debit(self, x):
        self.debits.append(x)

    def credit(self, x):
        self.credits.append(x)

    def _balance(self):
        return float(sum(self.debits) - sum(self.credits))
    
    @property
    def balance(self):
        return self._balance()
   
    def __repr__(self):
        return "%s('%s', %.1f)" % (self.__class__.__name__,
                                   self.name,
                                   self.balance)    
    
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
        return -1 * self._balance()
    
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

# Link account classes to text strings, allows jsons below 
CLASS_CONSTRUCTORS = dict(assets=Asset,
                          equity=Equity,
                          liabilities=Liability,
                          expenses=Expense,
                          income=Income,
                          profit=Profit)

# Account names and entry/transaction catalog are the inputs
# These inputs can be a json
ACCOUNT_NAMES = dict(assets=['cash', 'inventory', 'receivables'], 
                     equity=['capital'],
                     liabilities=['payables'],
                     income=['sales'], 
                     expenses=['cogs'])
OPERATIONS = {"pay in capital": ("cash", "capital"),
              "buy goods": ("inventory", "cash"),
              "ship goods": ("cogs", "inventory"),
              "get payment": ("cash", "sales")
              } 

# Model

def init_accounts(account_names=ACCOUNT_NAMES,
                  constructors=CLASS_CONSTRUCTORS):
    return {v:constructors[kw](v) 
            for kw, values in account_names.items() for v in values} 

# Controller
    
def entry(accounts, debit_side, credit_side, amount):
    accounts[debit_side].debit(amount)
    accounts[credit_side].credit(amount)
    return accounts

def process(accounts, event, catalog=OPERATIONS):
    action_name, amount = event
    acc1, acc2 = catalog[action_name]
    return entry(accounts, acc1, acc2 , amount)
       
# Reference functions 

def account_list(accounts):
    return accounts.values()

def account_list_by_type(accounts, cls):
    return [a for a in accounts.values() if isinstance(a, cls)]

def combine_dicts(a, b):
    c = a.copy()
    c.update(b)
    return c

def to_dict(account_list):
    return {a.name:a for a in account_list}

def account_dict_by_type(accounts, cls):
    return to_dict(account_list_by_type(accounts, cls))

def total(account_list):
    return sum(map(lambda x: x.balance, account_list))

def account_sum(accounts, cls):
    return total(account_list_by_type(accounts, cls))

def sum_debit(accounts):
    return account_sum(accounts, DebitAccount)

def sum_credit(accounts):
    return account_sum(accounts, CreditAccount)    

def has_identity(accounts):    
    return sum_debit(accounts) == sum_credit(accounts)

def values(accounts):
    return {k: v.balance for k,v in accounts.items()}

# End-of period transformations 

def profit_value(accounts):
    return account_sum(accounts, Income) - account_sum(accounts, Expense)

def profit_account(accounts):
    return Profit('profit', profit_value(accounts))    

def balance_sheet(accounts):
    result = account_dict_by_type(accounts, BalanceSheet)
    result['profit'] = profit_account(accounts)
    return result

# Groups of accounts

def group_dict(accounts, group_names, constructors=CLASS_CONSTRUCTORS):
    _accounts = account_list(accounts)
    result = collections.OrderedDict([(g, []) for g in group_names])
    for g in group_names:
        cls = constructors.get(g)
        for a in _accounts:
            if isinstance(a, cls):
                result[g].append(a)
    return result               

#  CLI tables in pure Python 

def balance_to_string(accounts):
    b = balance_sheet(accounts)
    a1 = group_dict(b, ["assets"])
    a2 = group_dict(b, ["equity", "liabilities"])
    xs = to_table_rows(a1)
    ys = to_table_rows(a2)
    return as_columns(list(xs), list(ys))

def pl_to_string(accounts):
    p = profit_account(accounts)
    z = combine_dicts(accounts, {'profit':p})
    g = group_dict(z, ['income', 'expenses', 'profit'])
    return newlined(to_table_rows(g))

# CLI helpers: to_table_rows and as_columns

def to_table_rows(d):
    return as_strings(make_rows(d))

def numeric(x):
    return '%.1f' % (abs(x) if x == 0 else x)

def along(a, b, padding):
    return a + " "*padding + b

def newlined(lines):
    return "\n".join(lines) 

def max_width(xs):
    return max(map(len, xs)) 

def coln(rows, i):
    return [x[i] for x in rows]

def make_rows(account_groups_dict, space = "  "):
    for key, accounts in account_groups_dict.items(): 
        yield key.capitalize(), ""
        for acc in accounts:
            text =  space + acc.name.capitalize()
            value = numeric(acc.balance)
            yield text, value   

def as_strings(rows, padding=2):
    rows = list(rows)
    n1 = max_width(coln(rows, 0))
    n2 = max_width(coln(rows, 1))
    def fmt(row):
        a = row[0].ljust(n1) 
        b = row[1].rjust(n2)
        return along(a, b, padding) 
    return map(fmt, rows)

def as_columns(xs, ys, padding = 4):
    w1 = max_width(xs)
    offset = lambda x: x.ljust(w1)     
    gen = itertools.zip_longest(xs, ys, fillvalue="")
    lines = [along(offset(x), y, padding) for x, y in gen]
    return newlined(lines) 


if '__main__' == __name__:
    accounts = init_accounts()
    events = [("pay in capital", 100.5), 
              ("buy goods", 75), 
              ("ship goods", 50),
              ("get payment", 60)
               ]    
    for event in events: 
        process(accounts, event)
        assert has_identity(accounts)
        assert has_identity(balance_sheet(accounts))
    
    assert values(accounts) == {'capital': 100.5,
                                'cash': 85.5,
                                'cogs': 50.0,
                                'inventory': 25.0,
                                'payables': 0.0,
                                'receivables': 0.0,
                                'sales': 60.0}
    assert profit_value(accounts) == 10
    assert has_identity(accounts)    
    assert has_identity(balance_sheet(accounts))

    print("This raw balance:\n")
    print(values(accounts)) 
    print("\nCan be viewed as:\n")
    print(balance_to_string(accounts))  
    print("\nand:\n")
    print(pl_to_string(accounts))
