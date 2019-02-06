"""Minimal double-entry accounting example with 
   T-accounts, legder and accounting entries catalog.
"""
from prettytable import PrettyTable, NONE, ALL

class Account:
    def __init__(self, name, start_balance=0):
        if start_balance != 0:
            raise NotImplementedError("Use DebitAccount or CreditAccount")
        self.name = name
        self.debits = []
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
    def __init__(self, name, start_balance=0.0):
        super().__init__(name)
        self.debits = [start_balance]
        

class CreditAccount(Account):
    def __init__(self, name, start_balance=0.0):
        super().__init__(name)
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

CLASS_CONSTRUCTORS = dict(assets=Asset,
                          equity=Equity,
                          liabilities=Liability,
                          expenses=Expense,
                          income=Income,
                          profit=Profit)
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

def init_accounts(account_names=ACCOUNT_NAMES,
                  constructors=CLASS_CONSTRUCTORS):
    return {v:constructors[kw](v) 
    for kw, values in account_names.items() for v in values} 
    
def entry(accounts, debit_side, credit_side, amount):
    accounts[debit_side].debit(amount)
    accounts[credit_side].credit(amount)
    return accounts

def process(accounts, event, catalog=OPERATIONS):
    action_name, amount = event
    acc1, acc2 = catalog[action_name]
    return entry(accounts, acc1, acc2 , amount)
       
# Reference functions 

def profit(accounts):
    return account_sum(accounts, Income) - account_sum(accounts, Expense)

def account_list(accounts):
    return accounts.values()

def account_list_by_type(accounts, cls):
    return [a for a in accounts.values() if isinstance(a, cls)]

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

def add_profit_account(accounts):
    _accounts = accounts.copy()
    p = Profit('profit', profit(_accounts))
    _accounts['profit'] = p
    return _accounts    

def balance_sheet(accounts):
    return account_dict_by_type(add_profit_account(accounts), BalanceSheet)

# Groups by keyword like 'assets', 'equity'

def account_groups(accounts, template_dict=CLASS_CONSTRUCTORS):
    return {kw:account_dict_by_type(accounts, cls) for kw, cls in template_dict.items()}

def account_groups_by_keyword(accounts, keywords):
    return {kw:accounts
            for kw, accounts in account_groups(accounts).items()
            if kw in keywords}
    
def group_dict(accounts, keywords):
    return account_groups_by_keyword(accounts, keywords)

# Reporting groups

def assets_dict(accounts):
    return group_dict(accounts, ["assets"])

def el_dict(accounts):
    return group_dict(accounts, ["equity", "liabilities"])

def p_and_l_dict(accounts):
    return group_dict(balance_sheet(accounts),
                      ["income", "expenses", "profit"])

#  CLI plotting

def fmt_value(x):
    """Prevent -0.0 float value"""
    return abs(x) if x == 0 else x 

def make_rows(accounts):
    offset = "  "    
    for key in accounts.keys():
        yield key.capitalize(), ""
        for k, acc in accounts[key].items():
            yield offset+k.capitalize(), fmt_value(acc.balance)

def make_table(rows):
    t = PrettyTable(field_names=['name','value'])
    t.align['name']='l'
    t.align['value']='r'
    t.header = False
    t.hrules = NONE
    t.vrules = NONE
    t.left_padding_width = 0
    t.right_padding_width = 0
    t.float_format=".1"
    for row in rows:
        t.add_row(row)
    return t

def make_table_by_keywords(accounts, keywords):
    return make_table(make_rows(group_dict(accounts, keywords)))

# CLI reports
        
def print_balance(accounts):
    _accounts = balance_sheet(accounts)
    ta = sum_credit(_accounts)
    tp = sum_debit(_accounts)
    a = make_table(make_rows(assets_dict(accounts)))
    b = make_table(make_rows(el_dict(accounts)))
    t = PrettyTable(field_names=['col1','col2'])
    t.align['col1']='r'
    t.align['col2']='r'
    t.left_padding_width = 0
    t.right_padding_width = 0
    t.header = False
    t.hrules = ALL
    t.vrules = NONE 
    t.junction_char = " "
    t.add_row([a,b])
    #FIXME: more formatting of the totals
    #t.add_row(["", f'Total equity: {tp}']) 
    t.add_row([f'Total: {ta}', f'Total: {tp}'])    
    return t                          

def print_pl(accounts):
    raise NotImplementedError

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
    print(values(accounts)) 
    t = print_balance(accounts)    
    print(t)
    
    assert values(accounts) == {'capital': 100.5,
 'cash': 85.5,
 'cogs': 50.0,
 'inventory': 25.0,
 'payables': 0.0,
 'receivables': 0.0,
 'sales': 60.0}
    assert profit(accounts) == 10
    assert has_identity(accounts)