

Требуется реализовать простой класс, хранящий информацию о банковском аккаунте.

Класс должен иметь публичные
* Конструктор от `Int`, создающий аккаунт с соответствующим балансом
* Свойство `balance`, равное текущему балансу 
* Метод `deposit(amount: Int)`, позволяющий увеличить баланс на `amount`
* Метод `withdraw(amount: Int)`, позволяющий уменьшить баланс на `amount`


Пример использования

```kotlin
val account = BankAccount(amount = 100)
println(account.balance) // 100

account.deposit(100)
println(account.balance) // 200

account.withdraw(150)
println(account.balance) // 50
```

Так же требуется при изменении баланса вызывать функцию `logTransaction` для логирования.
