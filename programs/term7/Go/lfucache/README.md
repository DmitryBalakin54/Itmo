# LFU cache

---

В данном задании необходимо поддержать кэш, используя алгоритм замещения [least frequently used](https://en.wikipedia.org/wiki/Least_frequently_used).

Перед выполнением этого задания рекомендуется решить [похожую задачу на LeetCode](https://leetcode.com/problems/lfu-cache/).

---

## Описание

Для решения этой задачи вам понадобится связный список. Как вы знаете, встроенный список из стандарной библиотеки Go
имеет ряд недостатков, например, он хранит элементы как `any`, что делает его небезопасным и потенциально медленным

```go
// Element is an element of a linked list.
type Element struct {
	...
	// The value stored with this element.
	Value any
}
```

Поэтому в рамках этого задания вам предлагается написать свой [связный список](./internal/linkedlist/list.go), чтобы решить
основную задачу:

```go
package lfu

import (
	"errors"
	"iter"
)

var ErrKeyNotFound = errors.New("key not found")

const DefaultCapacity = 5

// Cache
// O(capacity) memory
type Cache[K comparable, V any] interface {
	// Get returns the value of the key if the key exists in the cache,
	// otherwise, returns ErrKeyNotFound.
	//
	// O(1), not amortized
	Get(key K) (V, error)

	// Put updates the value of the key if present, or inserts the key if not already present.
	//
	// When the cache reaches its capacity, it should invalidate and remove the least frequently used key
	// before inserting a new item. For this problem, when there is a tie
	// (i.e., two or more keys with the same frequency), the least recently used key would be invalidated.
	//
	// O(1), not amortized
	Put(key K, value V)

	// All returns the iterator in descending order of frequency.
	// If two or more keys have the same frequency, the most recently used key will be listed first.
	//
	// O(capacity), not amortized
	All() iter.Seq2[K, V]

	// Size returns the cache size.
	//
	// O(1), not amortized
	Size() int

	// Capacity returns the cache capacity.
	//
	// O(1), not amortized
	Capacity() int

	// GetKeyFrequency returns the element's frequency if the key exists in the cache,
	// otherwise, returns ErrKeyNotFound.
	//
	// O(1), not amortized
	GetKeyFrequency(key K) (int, error)
}

// cacheImpl represents LFU cache implementation
type cacheImpl[K comparable, V any] struct{}

// New initializes the cache with the given capacity.
// If no capacity is provided, the cache will use DefaultCapacity.
func New[K comparable, V any](capacity ...int) *cacheImpl[K, V] {
	return new(cacheImpl[K, V])
}
```

## Сдача
* Решение необходимо реализовать в файлах [lfucache.go](./internal/lfucache/lfucache.go) и [list.go](./internal/linkedlist/list.go) 
* Открыть pull request из ветки `hw` в ветку `main` **вашего репозитория**
* В описании PR заполнить количество часов, которые вы потратили на это задание
* Отправить заявку на ревью в соответствующей форме
* Изменять файлы в директории [.github](.github) запрещено
* Время для дедлайна фиксируется отправкой формы
* Запрещено отправлять форму до того, как CI успешно завершится

## Особенности реализации
* Не все пакеты можно использовать
* Используйте тесты, чтобы понять недосказанности
* Подумайте, как можно использовать вершину связного списка сразу в нескольких местах
* Старайтесь не создавать новые указатели, если это возможно, например, при перемещении ноды между списками
* Если у вас не проходят тесты на скорость, возможно, вы используете слишком много лишних указателей в вашем решении


## Скрипты
Для запуска скриптов на курсе необходимо установить [go-task](https://taskfile.dev/docs/installation)

`go install github.com/go-task/task/v3/cmd/task@latest`

Перед выполнением задания не забудьте выполнить:

```bash 
task update
```

Запустить линтер:
```bash 
task lint
```

Запустить тесты:
```bash
task test
``` 

Обновить файлы задания
```bash
task update
```

Принудительно обновить файлы задания
```bash
task force-update
```

Скрипты работают на Windows, однако при разработке на этой операционной системе
рекомендуется использовать [WSL](https://learn.microsoft.com/en-us/windows/wsl/install)
