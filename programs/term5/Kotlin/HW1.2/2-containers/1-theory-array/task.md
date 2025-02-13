

Это базовый контейнер.

Он не расширяемый в плане новых ячеек, но вот значения в них меняются

### Как создать массив?

Конструктор:

```kotlin
val array: Array<String> = Array<String>(10) { index -> "$index" }

val inArray1: IntArray = IntArray(10)
val inArray2: IntArray = IntArray(10) { index -> index }
```

Вместо литерала в котлине есть следующее соглашение:
```kotlin
val stringArray: Array<String> = arrayOf<String>("1", "2", "3")
val intArray: IntArray = intArrayOf(1, 2, 3)
```

### Как использовать?

Изучите методы класса [самостоятельно](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-array/).
