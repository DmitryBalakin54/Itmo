import io.mockk.every
import io.mockk.mockkStatic
import io.mockk.verify
import org.junit.Assert
import org.junit.Test
import org.junit.Before
import org.junit.FixMethodOrder
import org.junit.runners.MethodSorters
import java.lang.reflect.Modifier

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class Tests {
    @Before
    fun before() {
        mockkStatic(::logTransaction)
        every { logTransaction(any(), any()) } returns Unit
    }

    @Test
    fun test1ConstructorWithAmount() {
        val account = BankAccount(42)
        Assert.assertEquals("Bank account should has correct balance", 42, account.balance)
        val account2 = BankAccount(0)
        Assert.assertEquals("Bank account should has correct balance", 0, account2.balance)
    }

    @Test
    fun test2ConstructorFailsWithNegativeBalance() {
        assertThrows("Bank account can't has negative balance") {
            BankAccount(-12)
        }
    }

    @Test
    fun test3DepositShouldWorkCorrectly() {
        val account = BankAccount(0)

        account.deposit(100)
        Assert.assertEquals(
            "Account with 0 basic balance should have 100 balance after deposit 100",
            100,
            account.balance
        )
        verify { logTransaction(eq(0), eq(100)) }

        assertThrows("Should be impossible to deposit a negative or zero amount") { account.deposit(0) }
        assertThrows("Should be impossible to deposit a negative or zero amount") { account.deposit(-1) }
        Assert.assertEquals(
            "Account with 100 balance should have same balance after incorrect deposit",
            100,
            account.balance
        )

        account.deposit(122)
        Assert.assertEquals("Account with 100 balance should have balance 222 after deposit 122", 222, account.balance)
        verify { logTransaction(eq(100), eq(222)) }
    }

    @Test
    fun test4WithdrawShouldWorkCorrectly() {
        val account = BankAccount(1000)

        account.withdraw(300)
        Assert.assertEquals(
            "Account with balance 1000 should has correct balance after withdraw 300",
            700,
            account.balance
        )
        verify { logTransaction(eq(1000), eq(700)) }

        assertThrows("Should be impossible to withdraw a negative or zero amount") { account.withdraw(-3) }
        assertThrows("Should be impossible to withdraw a negative or zero amount") { account.withdraw(0) }
        assertThrows("Should be impossible to withdraw amount greater than current balance") { account.withdraw(701) }


        account.withdraw(700)
        Assert.assertEquals(
            "Account with balance 1000 should has correct balance after withdraw 700",
            0,
            account.balance
        )
        verify { logTransaction(eq(700), eq(0)) }
    }

    @Test
    fun test5BalanceShouldHaveNotPublicSetter() {
        val javaMethod = BankAccount::class.java.methods.find { it.name == "setBalance" } ?: return
        Assert.assertFalse(
            "BankAccount should have private setter of balance property",
            Modifier.isPublic(javaMethod.modifiers)
        )
    }
}
