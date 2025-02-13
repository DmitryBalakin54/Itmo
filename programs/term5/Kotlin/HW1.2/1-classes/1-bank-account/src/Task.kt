class BankAccount(balance: Int) {

    var balance: Int = 0
        private set(newVal) {
            logTransaction(field, newVal)
            field = newVal
        }

    init {
        require(balance >= 0) { "Bank account can't has negative balance" }
        this.balance = balance
    }
    fun deposit(amount: Int) {
        require(amount > 0) { "Deposit must be not negative" }
        balance += amount
    }

    fun withdraw(amount: Int) {
        require(amount > 0) { "Withdraw must be not negative" }
        require(amount <= balance) { "Withdraw must be less on equal balance" }
        balance -= amount
    }
}

fun logTransaction(from: Int, to: Int) {
    println("$from -> $to")
}
