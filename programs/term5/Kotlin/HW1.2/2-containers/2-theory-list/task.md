

Спики бывают изменяемые и не изменяемые. В Kotlin это 2 отдельных типа.

### Как создать?

Конструктор класса реализации

```kotlin
import java.util.*

val arrayList: MutableList<Int> = ArrayList<Int>()
val linkedList: MutableList<Int> = LinkedList<Int>()
```

Фабричная функция `List`/`MutableList`:

```kotlin
val list: List<Int> = List(10) { index -> index }
val mutableList: MutableList<Int> = MutableList(10) { index -> index }
```

Билдер:

```kotlin
val list = buildList<Int> {
    add(1)
    add(2)
    add(3)
}
```

Вместо литерала:

```kotlin
val list: List<Int> = listOf(1, 2, 3)
val mutableList: MutableList<Int> = mutableListOf(1, 2, 3)
```

Приведение между мутабельными (изменяемыми) и имутабельными (неизменяемыми) типами:

```kotlin
val mutableList: MutableList<Int> = listOf(1, 2, 3).toMutableList()
val immutableList1: List<Int> = mutableListOf(1, 2, 3).toList()

val immutableList2: List<Int> = mutableListOf(1, 2, 3) // upper cast
```

### Как использовать?

Изучите методы классов самостоятельно.
