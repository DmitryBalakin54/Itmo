# Digest

## Необходимо поддержать следующие функции

```go
// GetCharByIndex returns the i-th character from the given string.
func GetCharByIndex(str string, idx int) rune {
	panic("not implemented")
}

// GetStringBySliceOfIndexes returns a string formed by concatenating specific characters from the input string based
// on the provided indexes.
func GetStringBySliceOfIndexes(str string, indexes []int) string {
	panic("not implemented")
}

// ShiftPointer shifts the given pointer by the specified number of bytes using unsafe.Add.
func ShiftPointer(pointer **int, shift int) {
	panic("not implemented")
}

// IsComplexEqual compares two complex numbers and determines if they are equal.
func IsComplexEqual(a, b complex128) bool {
	panic("not implemented")
}

// GetRootsOfQuadraticEquation returns two roots of a quadratic equation ax^2 + bx + c = 0.
func GetRootsOfQuadraticEquation(a, b, c float64) (complex128, complex128) {
	panic("not implemented")
}

// Sort sorts in-place the given slice of integers in ascending order.
func Sort(source []int) {
	panic("not implemented")
}

// ReverseSliceOne in-place reverses the order of elements in the given slice.
func ReverseSliceOne(s []int) {
	panic("not implemented")
}

// ReverseSliceTwo returns a new slice of integers with elements in reverse order compared to the input slice.
// The original slice remains unmodified.
func ReverseSliceTwo(s []int) []int {
	panic("not implemented")
}

// SwapPointers swaps the values of two pointers.
func SwapPointers(a, b *int) {
	panic("not implemented")
}

// IsSliceEqual compares two slices of integers and returns true if they contain the same elements in the same order.
func IsSliceEqual(a, b []int) bool {
	panic("not implemented")
}

// DeleteByIndex deletes the element at the specified index from the slice and returns a new slice.
// The original slice remains unmodified.
func DeleteByIndex(s []int, idx int) []int {
	panic("not implemented")
}
```

## Особенности реализации
* Не все пакеты можно использовать
* Используйте тесты чтобы понять недосказанности

## Сдача
* Все функции реализовать в файле [main.go](./internal/digest/main.go)
* Открыть pull request из ветки `hw` в ветку `main` **вашего репозитория**
* В описании PR заполнить количество часов, которые вы потратили на это задание
* Отправить заявку на ревью в соответствующей форме
* Изменять файлы в директории [.github](.github) запрещено
* Время для дедлайна фиксируется отправкой формы
* Запрещено отправлять форму до того, как CI успешно завершится 

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
