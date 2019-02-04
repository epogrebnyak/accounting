class Account:
    def __init__(self, name):
        self.name = name
        self._debits = []
        self._credits = []
        
    def debit(self, x):
        self._debits.append(x)

    def credit(self, x):
        self._credits.append(x)
    
    def __repr__(self):
        return f"{self.name}: {round(self.balance, 2)}"
        

class DebitAccount(Account):
    @property
    def balance(self):
        return sum(self._debits) - sum(self._credits)
    
    
class CreditAccount(Account):
    @property
    def balance(self):
        return sum(self._credits) - sum(self._debits)
    
class Asset(DebitAccount):
    pass
        
class Equity(CreditAccount):
    pass

class Liability(CreditAccount):
    pass
        
class Expense(DebitAccount):
    pass

class Income(CreditAccount):
    pass

class Profit(CreditAccount):
    pass

BALANCE_ACCOUNTS = Asset, Equity, Liability, Profit 
REVENUE_AND_EXPENSE_ACCOUNTS = Expense, Income
ALL_ACCOUNTS = BALANCE_ACCOUNTS + REVENUE_AND_EXPENSE_ACCOUNTS

def get_constructor(cls_str):
    return {cls.__name__: cls for cls in ALL_ACCOUNTS}[cls_str]

def isin(account, class_list):
    return any([isinstance(account, x) for x in class_list])
    
   
SHORT_TO_LONG_NAMES = dict([
    ('cash', 'Cash and equivalents'),
    ('cogs', 'Cost of goods sold'),
    ('sales', 'Sales'),
    ('paya', 'Accounts payable'),
    ('cap', 'Paid-in capital'),
    ('reca', 'Accounts receivable'),  
    ('invy', 'Inventories'),
    ('admin', 'Admin expenses')
    ])

ACCOUNT_NAMES = {Asset: ['cash', 'reca', 'invy'],
                 Equity: ['cap'],
                 Liability: ['paya'],
                 Expense: ['cogs', 'admin'],
                 Income: ['sales']
                 }

def account_list(account_names):
    return account_dict(account_names).values()

def account_dict(account_names):
    return {v:key(SHORT_TO_LONG_NAMES[v]) 
            for key, values in account_names.items() 
            for v in values}

def mutate(accounts, debit_acc, credit_acc, amount):
    accounts[debit_acc].debit(amount)
    accounts[credit_acc].credit(amount)

TRANSACTIONS = {
      'Paid capital': ('cash', 'cap'),
      'Obtained inventory': ('invy', 'paya'),
      'Paid my invoice': ('paya', 'cash'),
      'Delievered goods sold': ('cogs', 'invy'),
      'Invoiced for goods sold': ('reca', 'sales'),
      'Customer paid invoice': ('cash', 'reca'),
      'Incured admin expenses': ('admin', 'paya')
}


def transact(accounts, transaction_name, amount):
    debit_acc, credit_acc = TRANSACTIONS[transaction_name]
    mutate(accounts, debit_acc, credit_acc, amount)
    print(transaction_name+",", "value:", amount)
    
def profit(accounts):
    p = Profit("Earnings before tax")
    p.credit(total(accounts, Income) - total(accounts, Expense))    
    return {'profit': p}

def total(accounts, cls):
    return sum([acc.balance for acc in account_by_type(accounts, cls)])

def account_by_type(accounts, cls):
    return [a for a in accounts.values() if isinstance(a, cls)]
 
def net_balance(accounts):
    _accounts = {key: account for key, account in accounts.items() 
                      if not isin(account, [Expense, Income])}
    p = profit(accounts)
    _accounts.update(p)
    return _accounts

def filled(text, value):
    return '{:<24s}'.format(text) + '{:12.1f}'.format(value)

def display(accounts, class_list):    
    def plural(key):
        key = key.lower()
        return {"asset": "assets", "liability": "liabilities"}.get(key, key)       
    
    for cls in class_list:
        header = plural(cls.__name__).capitalize() 
        x = total(accounts, cls)
        print(filled(header, x))        
        for a in account_by_type(accounts, cls):
            print(filled("   "+a.name, a.balance))

def display_balance(accounts):
    print("\nBalance sheet")
    display(net_balance(accounts), 
            class_list=(Asset, Equity, Profit, Liability))

def display_pl(accounts):
    print("\nFinancial statement")
    display(accounts, [Income, Expense])
    display(profit(accounts), [Profit])


def sum_groups(accounts, class_list):
    return sum([total(accounts, cls) for cls in class_list])


def assert_balance(accounts):
    assert sum_groups(accounts, [Asset, Expense]) == \
           sum_groups(accounts, [Equity, Liability, Income, Profit])

def assert_balance_net(accounts):
    assert_balance(net_balance(accounts))

if __name__ == "__main__":   
    amazon = account_dict(ACCOUNT_NAMES)
    events = [('Paid capital', 100),
              ('Obtained inventory', 70),
              ('Paid my invoice', 70),
              ('Delievered goods sold', 50),
              ('Invoiced for goods sold', 60),
              ('Customer paid invoice', 60),
              ('Incured admin expenses', 2),
              ('Paid my invoice', 2)]
    for event in events:
        transact(amazon, *event)
    
    assert_balance(amazon)
    assert_balance_net(amazon)
    display_balance(amazon)
    display_pl(amazon)    
