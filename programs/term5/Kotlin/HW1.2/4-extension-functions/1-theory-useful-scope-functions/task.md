

Ознакомитесь с документацией на страничке: https://kotlinlang.org/docs/scope-functions.html

Советы по использованию:

* `let` - помогает при работе с nullable типами
  ```kotlin
  nullable?.let { notNull -> doSome(notNull) }
  // или
  nullable?.let { notNull -> doSome(notNull) } ?: valueIfNull
  ```

* `apply` - помогает при работе с билдерами
  ```kotlin
  builder.apply {
      withColor("#FFFF")
      withOffset(50)
  }
  ```
  Билдер должен быть изменяемым, то для него не должно требоваться каждый раз сохранять новую версию его состояния.

* `also` - позволяет использовать передать значение в функцию, не используя результат этой функции
  ```kotlin
  val value = getValue().also { println("[debug] $it") }
  ```

* `run`/`with` - можно использовать для захвата контекста в функциях с двумя ресиверами.
  ```kotlin
  class Context {
      fun Int.toSuperString(): String = let { i ->  "$i:$i" }
  }
  
  fun main() {
      val context = Context()
      context.run { 1.toSuperString() }
      with(context) { 1.toSuperString() }
  }
  ```
