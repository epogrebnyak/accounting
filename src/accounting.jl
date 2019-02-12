mutable struct TAccount
    debits :: Vector 
    credits :: Vector
end

mutable struct CreditAccount
    debits :: Vector 
    credits :: Vector
end

mutable struct DebitAccount
    debits :: Vector 
    credits :: Vector
end

# In Julia, can one inherit types from DebitAccount and CreditAccount?

function debit_sum(acc)
    return sum(acc.debits) - sum(acc.credits)
end

function balance(acc :: DebitAccount)
    return debit_sum(acc)
end    

function balance(acc :: CreditAccount)
    return -debit_sum(acc)
end  
