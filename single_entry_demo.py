class Account:
    def __init__(self, name):
        self.name = name
        self._start = 0.0
        self._increases = []
        self._decreases = []
        
    def set_start_balance(self, x):
        self._start = x
        
    def increase(self, x):
        self._increases.append(x)

    def decrease(self, x):
        self._decreases.append(x)
    
    @property    
    def balance(self):
        return self._start + sum(self._increases) - sum(self._decreases) 
        
    def __repr__(self):
        return f"{self.name}: {round(self.balance, 2)}"
        
class Liability(Account):
    pass

class Asset(Account):
    pass
        
class Equity(Account):
    pass
        
class Expense(Account):
    pass

class Income(Account):
    pass

class Profit(Account):
    pass

BALANCE_ACCOUNTS = Asset, Equity, Liability, Profit 
REVENUE_AND_EXPENSE_ACCOUNTS = Expense, Income
ALL_ACCOUNTS = BALANCE_ACCOUNTS + REVENUE_AND_EXPENSE_ACCOUNTS
DEBIT_SIDE_ACCOUNTS = Asset, Expense

def get_constructor(cls_str):
    return {cls.__name__: cls for cls in ALL_ACCOUNTS}[cls_str]

def isin(account, class_list):
    return any([isinstance(account, x) for x in class_list])
    
def is_debit_side(account):
    return isin(account, DEBIT_SIDE_ACCOUNTS)
    
SHORT_TO_LONG_NAMES = dict([
    ('cash', 'Cash and equivalents'),
    ('cogs', 'Cost of goods sold'),
    ('sales', 'Sales'),
    ('paya', 'Accounts payable'),
    ('cap', 'Paid-in capital'),
    ('reca', 'Accounts receivable'),  
    ('invy', 'Inventories'),
    ('admin', 'Administrative expenses')
    ])

ACCOUNT_NAMES = {'Asset': ['cash', 'reca', 'invy'],
                 'Equity': ['cap'],
                 'Liability': ['paya'],
                 'Expense': ['cogs', 'admin'],
                 'Income': ['sales']
                 }

def account_list(account_names):
    return account_dict(account_names).values()

def account_dict(account_names):
    return {v:get_constructor(key)(SHORT_TO_LONG_NAMES[v]) 
            for key, values in account_names.items() 
            for v in values}

def is_same_side(acc1, acc2):
    return is_debit_side(acc1) == is_debit_side(acc2)

def mov(x, from_account, to_account):
    assert is_same_side(from_account, to_account)
    from_account.decrease(x)
    to_account.increase(x)

def inc(x, acc1, acc2):
    assert not is_same_side(acc1, acc2)
    acc1.increase(x)
    acc2.increase(x)

def dec(x, acc1, acc2):
    inc(-x, acc1, acc2)


MOVE = {
     'obtain_inventory': 
         ('cash', 'invy', "Swapped cash for inventory"),
     'ship_goods': 
         ('invy', 'cogs', 'Goods shipped to customer'),
     'customer_paid': 
         ('reca', 'cash', 'Customer paid cash on invoice')
}

INCREASE = {
     'add_capital': 
         ('cash', 'cap', 'Shareholder added capital in cash'),
     'incur_admin_expense':    
         ('paya', 'admin', 'Incurred administrative expenses'),    
     'customer_invoiced': 
         ('reca', 'sales', 'Customer invoiced for purchase of goods')
}

DECREASE = {
     'paid_admin_expense': 
         ('cash', 'paya', 'Paid on outstanding administrative expense')
        }
   
    
def get_args(transaction_name):
    trans_dicts = MOVE, INCREASE, DECREASE
    funcs = mov, inc, dec
    for tdict, func in zip(trans_dicts, funcs):
        if transaction_name in tdict.keys():
            return tdict[transaction_name], func   

def transact(accounts, transaction_name, amount):
    (acc_name1, acc_name2, note), f = get_args(transaction_name)
    f(amount, accounts[acc_name1], accounts[acc_name2])
    print(note+",", "value:", amount)
    
def profit(accounts):
    p = Profit("Earnings before tax")
    p.set_start_balance(total(accounts, Income) - total(accounts, Expense))    
    return {p.name: p}

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
    print("\nBalance sheeet")
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
    events = [('add_capital', 100),
              ('obtain_inventory', 70),
              ('customer_invoiced', 60),
              ('ship_goods', 50),
              ('customer_paid', 60),
              ('incur_admin_expense', 2),
              ('paid_admin_expense', 2)]
    
    for event in events:
        transact(amazon, *event)
    
    assert_balance(amazon)
    assert_balance_net(amazon)
    display_balance(amazon)
    display_pl(amazon)    

#('Add capital', 'cash', 'cap', 100)
#
#def provodka(accounts, debit_acc, credit_acc, amount):
#    if is_debit_account(debit_acc)
    
