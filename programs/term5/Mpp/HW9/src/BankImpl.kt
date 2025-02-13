import java.util.concurrent.locks.ReentrantReadWriteLock
import kotlin.concurrent.withLock

/**
 * @author Balakin Dmitry
 */
class BankImpl(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    override fun getAmount(index: Int): Long {
        val account = accounts[index]
        account.lock.readLock().withLock {
            return account.amount
        }
    }

    override val totalAmount: Long
        get() {
            accounts.forEach { it.lock.readLock().lock() }
            try {
                return accounts.sumOf { it.amount }
            } finally {
                accounts.forEach { it.lock.readLock().unlock() }
            }
        }

    override fun deposit(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }

        val account = accounts[index]
        account.lock.writeLock().withLock {
            check(amount <= Bank.MAX_AMOUNT && account.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }

            account.amount += amount

            return account.amount
        }
    }

    override fun withdraw(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }

        val account = accounts[index]
        account.lock.writeLock().withLock {
            check(account.amount - amount >= 0) { "Underflow" }

            account.amount -= amount

            return account.amount
        }
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }

        val (firstIndex, secondIndex) = if (fromIndex < toIndex) fromIndex to toIndex else toIndex to fromIndex
        val (first, second) = accounts[firstIndex] to accounts[secondIndex]

        first.lock.writeLock().withLock {
            second.lock.writeLock().withLock {
                val (from, to) = accounts[fromIndex] to accounts[toIndex]

                check(amount <= from.amount) { "Underflow" }
                check(amount <= Bank.MAX_AMOUNT && to.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }

                from.amount -= amount
                to.amount += amount
            }
        }
    }

    override fun consolidate(fromIndices: List<Int>, toIndex: Int) {
        require(fromIndices.isNotEmpty()) { "empty fromIndices" }
        require(fromIndices.distinct() == fromIndices) { "duplicates in fromIndices" }
        require(toIndex !in fromIndices) { "toIndex in fromIndices" }

        val lockedAccounts = (fromIndices + toIndex).distinct().sorted().map { accounts[it] }
        lockedAccounts.forEach { it.lock.writeLock().lock() }

        try {
            val to = accounts[toIndex]
            val amount = fromIndices.sumOf { accounts[it].amount }

            check(to.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }

            fromIndices.forEach { accounts[it].amount = 0 }
            to.amount += amount
        } finally {
            lockedAccounts.forEach { it.lock.writeLock().unlock() }
        }
    }

    class Account {
        var amount: Long = 0
        val lock = ReentrantReadWriteLock()
    }
}
