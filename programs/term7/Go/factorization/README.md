# Factorization


## Описание

В данном задании вам необходимо реализовать следующий интерфейс:

```go
var (
	// ErrFactorizationCancelled is returned when the factorization process is cancelled.
	ErrFactorizationCancelled = errors.New("cancelled")
	
	// ErrWriterInteraction is returned if an error occurs while interacting with the writer
	// triggering early termination.
	ErrWriterInteraction      = errors.New("writer interaction")
)

// Factorizer interface represents a concurrent prime factorization task with configurable workers.
// Thread safety and error handling are implemented as follows:
// - The provided writer must be thread-safe to handle concurrent writes from multiple workers.
// - Output uses '\n' for newlines.
// - Factorization has a time complexity of O(sqrt(n)) per number.
// - If an error occurs while writing to the writer, early termination is triggered across all workers.
type Factorizer interface {
	// Factorize performs factorization on a list of integers, writing the results to an io.Writer.
	// - numbers: the list of integers to factorize.
	// - writer: the io.Writer where factorization results are output.
	// Returns an error if the process is cancelled or if a writer error occurs.
	Factorize(ctx context.Context, numbers []int, writer io.Writer) error
}

type factorizerImpl struct {}

func New(opts ...FactorizeOption) (*factorizerImpl, error) {
	// ...
}

type FactorizeOption func(*factorizerImpl)

// WithFactorizationWorkers
// Defines the configuration for factorization workers.
func WithFactorizationWorkers(workers int) FactorizeOption {
	// ...
}

// WithWriteWorkers
// Defines the configuration for write workers.
func WithWriteWorkers(workers int) FactorizeOption {
	// ...
}
```

Количество воркеров передаётся в конструкторе с помощью паттерна [functional options](https://habr.com/ru/articles/575316/). При этом отдельно задаётся
количество воркеров как для факторизации `WithFactorizationWorkers`, так и для записи `WithWriteWorkers`.

В рамках задания рекомендуется использовать [этот](https://ru.wikipedia.org/wiki/Перебор_делителей) алгоритм факторизации.

## Пример

#### Input:

`[100, -17, 25, 38], 3`

#### Output:
```text
100 = 2 * 2 * 5 * 5
-17 = -1 * 17
38 = 2 * 19
25 = 5 * 5
```

## Сдача
* Решение необходимо реализовать в файле [fact.go](./internal/fact/fact.go)
* Открыть pull request из ветки `hw` в ветку `main` **вашего репозитория**
* В описании PR заполнить количество часов, которые вы потратили на это задание
* Отправить заявку на ревью в соответствующей форме
* Изменять файлы в директории [.github](.github) запрещено
* Время для дедлайна фиксируется отправкой формы
* Запрещено отправлять форму до того, как CI успешно завершится

## Особенности реализации
* Множители записываются от меньшего к большему
* Если число меньше нуля, добавляется множитель -1
* Используйте тесты, чтобы заполнить недосказанности
* Обратите внимание на пакеты, которые [разрешено использовать](./.golangci.yaml)
* В этом задании ожидается решение с использованием каналов
* В учебных целях запрещено использовать буферизированные каналы
* В учебных целях запрещено использовать мьютексы

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
