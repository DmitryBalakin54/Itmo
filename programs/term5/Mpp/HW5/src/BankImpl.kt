import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * Bank implementation.
 *
 * @author : Balakin Dmitry
 */
class BankImpl(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    override fun getAmount(index: Int): Long {
        val account = accounts[index]

        account.lock.withLock {
            return account.amount
        }
    }

    override val totalAmount: Long
        get() {
            val sortedAccounts = accounts.sortedBy { it.hashCode() }
            sortedAccounts.forEach { it.lock.lock() }

            try {
                return sortedAccounts.sumOf { it.amount }
            } finally {
                sortedAccounts.forEach { it.lock.unlock() }
            }
        }

    override fun deposit(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }

        val account = accounts[index]

        account.lock.withLock {
            check(!(amount > Bank.MAX_AMOUNT || account.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }

            account.amount += amount

            return account.amount
        }
    }

    override fun withdraw(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }

        val account = accounts[index]

        account.lock.withLock {
            check(account.amount - amount >= 0) { "Underflow" }

            account.amount -= amount

            return account.amount
        }
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }

        val from = accounts[fromIndex]
        val to = accounts[toIndex]
        val (first, second) = if (from.hashCode() < to.hashCode()) from to to else to to from

        first.lock.lock()
        second.lock.lock()

        try {
            check(amount <= from.amount) { "Underflow" }
            check(!(amount > Bank.MAX_AMOUNT || to.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }

            from.amount -= amount
            to.amount += amount
        } finally {
            second.lock.unlock()
            first.lock.unlock()
        }
    }

    class Account {
        var amount: Long = 0
        val lock = ReentrantLock()
    }
}