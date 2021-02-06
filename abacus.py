from dataclasses import dataclass, field
from typing import Dict, List

Value = int


@dataclass
class Account:
    debit_items: [Value] = field(default_factory=list)
    credit_items: [Value] = field(default_factory=list)


@dataclass
class DebitAccount(Account):
    def balance(self):
        return debit_balance(self)


class CreditAccount(Account):
    def balance(self):
        return -debit_balance(self)


def debit_balance(account):
    return sum(account.debit_items) - sum(account.credit_items)


@dataclass
class Chart:
    """Chart of accounts."""

    assets: List[str]
    expenses: List[str]
    liabilities: List[str]
    capital: List[str]
    income: List[str]
    
    @property
    def account_names(self):
        return self.debit_accounts + self.credit_accounts

    @property
    def debit_accounts(self):
        return self.assets + self.expenses

    @property
    def credit_accounts(self):
        return self.liabilities + self.capital + self.income

    def is_debit_account(self, account_name: str):
        return account_name in self.debit_accounts

    def is_credit_account(self, account_name: str):
        return account_name in self.credit_accounts


@dataclass
class Entry:
    value: Value
    debit: str
    credit: str


E = Entry


@dataclass
class Ledger:
    """Ledger to record transactions in accounts."""

    accounts: Dict[str, Account]

    def enter(self, debit, credit, value):
        self.accounts[debit].debit_items.append(value)
        self.accounts[credit].credit_items.append(value)

    def process(self, e: Entry):
        self.enter(**e.__dict__)


def make_ledger(chart: Chart) -> Ledger:
    accounts = dict()
    for name in chart.debit_accounts:
        accounts[name] = DebitAccount()
    for name in chart.credit_accounts:
        accounts[name] = CreditAccount()
    return Ledger(accounts)


def profit(ledger, chart):
    income, expenses = 0, 0
    for name in chart.income:
        income += ledger.accounts[name].balance()
    for name in chart.expenses:
        expenses += ledger.accounts[name].balance()
    return income - expenses


def balances(ledger, chart):
    res = {}
    for name in chart.assets + chart.liabilities + chart.capital:
        res[name] = ledger.accounts[name].balance()
    res["profit"] = profit(ledger, chart)
    return res


# from accounting import Chart, make_ledger, Entry, balances

# 1. Lay out a system of accounts, or "chart of accounts", CoA.
# CoA can be standardised in some countries or in certain businesses (eg banks).
chart = Chart(
    assets=["cash", "loans", "receivables", "fixed_assets"],
    expenses=["expenses"],
    liabilities=["current_accounts", "deposits", "payables"],
    capital=["equity", "retained_earnings"],
    income=["income"],
)

# 2. Create store of data, "a ledger"
L = make_ledger(chart)

# 3. Record transactions as double entries. A double entry system
#    always affects two accounts to keep balance coherent.
transactions = [
    # Recapitalisation with owner funds
    Entry(value=100, debit="cash", credit="equity"),
    # Loans provided to clients
    Entry(80, debit="loans", credit="cash"),
    # Interest due on loans accrued
    Entry(8, debit="receivables", credit="income"),
    # Сlients pay part of interest due
    Entry(5, debit="cash", credit="receivables"),
]
for t in transactions:
    L.process(t)

# 4. Check resulting sums by account.
assert balances(L, chart) == {
    "cash": 25,
    "loans": 80,
    "receivables": 3,
    "fixed_assets": 0,
    "current_accounts": 0,
    "deposits": 0,
    "payables": 0,
    "equity": 100,
    "retained_earnings": 0,
    "profit": 8,
}

def as_lines(xs, chart):    
    longest = max([len(x) for x in chart.account_names])
    l_offset = min(20, 3 + longest)
    r_offest = 4
    def line(text, value):
       return "  " + text.ljust(l_offset, ".") + str(value).rjust(r_offest) 
    bs = [line(renamer(x[0]), x[1]) for x in xs]
    return "\n".join(bs)


def str_assets(L, chart):
    xs = [(name, L.accounts[name].balance()) for name in chart.assets]
    return as_lines(xs, chart)


def nb(ledger, name):
    return (name, L.accounts[name].balance())


def str_passiv(L, chart):
    xs = (
        [nb(L, name) for name in chart.capital]
        + [("profit", profit(L, chart))]
        + [nb(L, name) for name in chart.liabilities]
    )
    return as_lines(xs, chart)


doc = """
debit cash credit equity 100
credit cash debit loans 80
credit income debit receivables 8
credit receivables debit cash 5
"""

def renamer(x):
    return x.replace("_", " ").capitalize()

def to_dict(command_line):
    xs = command_line.split()
    res = dict(value=int(xs[4]))
    res[xs[0]] = xs[1]
    res[xs[2]] = xs[3]
    return {xs[0]: xs[1], xs[2]: xs[3], "value": int(xs[4])}


commands = [to_dict(d) for d in doc.split("\n") if d]


print(balances(L, chart))
print("Assets")
print(str_assets(L, chart))
print()
print("Capital and liabilities")
print(str_passiv(L, chart))

