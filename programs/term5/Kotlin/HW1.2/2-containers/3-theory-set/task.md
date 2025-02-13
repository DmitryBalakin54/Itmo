

Множества бывают изменяемые и не изменяемые, как и списки.

### Как создать?

Конструктор класса реализации

```kotlin
    import java.util.*
    
    val hashSet: MutableSet<Int> = HashSet<Int>()
    val treeSet: MutableSet<Int> = TreeSet<Int>()
```

Билдер:

```kotlin
val set: Set<Int> = buildSet {
    add(1)
    add(2)
    add(3)
}
```

Вместо литерала:

```kotlin
val set: Set<Int> = setOf(1, 2, 3)
val mutableSet: MutableSet<Int> = mutableSetOf(1, 2, 3)
```

Приведение между мутабельными и имутабельными типами:

```kotlin
val mutableSet: MutableSet<Int> = setOf(1, 2, 3).toMutableSet()
val immutableSet1: Set<Int> = mutableSetOf(1, 2, 3).toSet()

val immutableSet2: Set<Int> = mutableSetOf(1, 2, 3) // upper cast
```

### Как использовать?

Изучите методы классов самостоятельно.
