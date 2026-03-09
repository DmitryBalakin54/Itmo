# Concurrency cryptography

(основано на реальных событиях)

## Предыстория

Вы работаете в большом банке. В этом банке есть хранилище банковских карт ваших пользователей.
Очевидно, что хранить и передавать банковские карты в открытом виде опасно, поэтому их шифруют.

Более того, есть стандарт [PCI-DSS](https://ru.wikipedia.org/wiki/PCI_DSS), который регламентирует дополнительные
правила безопасности для платёжных карт. Например, нельзя долго хранить [CVV-код карты](https://ru.wikipedia.org/wiki/CVV2), он вообще бывает динамическим.

В том числе иногда нужно ротировать ключи шифрования в вашем хранилище, чтобы повысить безопасность
за счёт ограничения потенциального ущерба в случае компрометации ключа.

В рамках этого задания вам предстоит реализовать функцию для многопоточного шифрования карт, для упрощения будем считать,
что все карты имеют длину 16 цифр. Предполагается, что эту функцию будут массово использовать при ротации ключа, чтобы зашифровать карты уже новым ключом

## Описание

Для шифрования карт необходимо использовать алгоритм [AES](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) в
режиме [GCM](https://ru.wikipedia.org/wiki/Galois/Counter_Mode).

> AES это довольно популярный алгоритм. Его реализация поддержана [на аппаратном уровне](https://github.com/golang/go/blob/c58d075e9a457fce92bdf60e2d1870c8c4df7dc5/src/crypto/internal/fips140/aes/gcm/gcm_amd64.s)
```go
hasAESGCMHardwareSupport = hasGCMAsmAMD64 || hasGCMAsmARM64 || hasGCMAsmS390X || hasGCMAsmPPC64
```

> Go AES-GCM можно вызывать из нескольких горутин, хоть это и неявная гарантия.
> > [Один раз это свойство даже сломали при реализации одного стандарта](https://github.com/golang-fips/go/issues/187). 
> В этом задании считаем, что инвариант выполняется

AES GCM mode входит в класс [AEAD алгоритмов](https://ru.wikipedia.org/wiki/AEAD-режим_блочного_шифрования), по своим
свойствам похож на [HMAC](https://ru.wikipedia.org/wiki/HMAC). Проще говоря, этот алгоритм не только шифрует исходные данные,
но ещё позволяет сохранить некоторую подпись (контрольную сумму), чтобы метаданные не могли подменить, например, при передаче по сети.
Мы будем этим пользоваться, неявно "зашивая" ID карты при шифровании.


---
Во многих режимах работы блочных шифров используются операции XOR. Для GCM (и других потоковых, то есть, где `plaintext xor keystream`) на каждый шифр необходимо генерировать уникальный инициализационный вектор (iv).
Рекомендуется ознакомиться [с этим примером](https://crypto.stackexchange.com/a/108) для лучшего понимания последствий использования неслучайного инициализационного вектора (nonce / iv)
в этом режиме.

> The nonce must be NonceSize() bytes long and unique for all time, for a given key.

Но нужно не просто генерировать уникальный инициализационный вектор, а делать это безопасно, чтобы злоумышленник не мог его предугадать.
Например, в "обычном" генераторе случайных чисел, зная seed и формулу можно угадать весь поток чисел.
Поэтому для криптографических задач используют генератор, который использует аппаратные источники энтропии: движение мыши, сетевой шум и т.д.

```go
import "crypto/rand"

if _, err := rand.Read(nonce); err != nil {
	// ...
}
```

На самом деле под капотом такой вызов делает системный вызов:

[linux](https://man7.org/linux/man-pages/man2/getrandom.2.html):
```go
r1, _, errno := syscall.Syscall(getrandomTrap,
    uintptr(unsafe.Pointer(unsafe.SliceData(p))),
    uintptr(len(p)),
    uintptr(flags))
```


[macOS](https://stackoverflow.com/a/23686867):
```go
syscall_syscall(abi.FuncPCABI0(libc_arc4random_buf_trampoline),
    uintptr(unsafe.Pointer(unsafe.SliceData(p))), uintptr(len(p)), 0)
```

В реальной жизни за такими системными вызовами могут быть скрыты неочевидные вещи. Например, на macOS используют
глобальный spinlock, что очень сильно бьёт по производительности. Именно поэтому в тестах задания для некоторых платформ используются эмуляторы.


В linux же, напротив, есть некоторые оптимизации

> If a request is  made to read a large number of bytes (more than 256), getrandom() will block until those bytes have been generated and transferred
from kernel memory to buf. Using getrandom() to read small buffers (<= 256 bytes) from the  urandom source is the preferred mode of usage.

Поэтому крайне важно писать бенчмарки и проверять их на целевой платформе.


## Задание

Однопоточная реализация задания, в ней намеренно могут быть допущены ошибки:

```go
func (c *crypterImpl) Encrypt(cards []Card, key []byte) ([]string, error) {
	block, _ := aes.NewCipher(key)
	gcm, _ := cipher.NewGCM(block)
	nonceSize := gcm.NonceSize()

	out := []string{}
	for i := 0; i < len(cards); i++ {
		nonce := make([]byte, gcm.NonceSize())

		if _, err = rand.Read(nonce); err != nil {
			return nil, err
		}

		buf := make([]byte, nonceSize, nonceSize+16+gcm.Overhead())
		copy(buf, nonce)

		result := gcm.Seal(
			buf,
			nonce,
			cards[i].Number[:],
			[]byte(cards[i].ID),
		)

		out = append(out, hex.EncodeToString(result))
	}

	return out, nil
}
```

Вам необходимо переделать эту реализацию на многопоточную. Количество воркеров передаётся в конструкторе с помощью
паттерна [functional options](https://habr.com/ru/articles/575316/).

```go
func New(opts ...CrypterOption) *crypterImpl {
	for _, opt := range opts {
		opt(c)
	}
}

type CrypterOption func(*crypterImpl)

func WithWorkers(workers int) CrypterOption {
	return func(s *crypterImpl) {
		s.workers = workers
	}
}
```

Пример расшифрования можно посмотреть [в тестах](https://github.com/igoroutine-courses/gonature.concrypto_tests/blob/main/tests/util_test.go#L52)

## Сдача
* Решение необходимо реализовать в файле [encrypt.go](./internal/cardcrypter/encrypt.go)
* Открыть pull request из ветки `hw` в ветку `main` **вашего репозитория**
* В описании PR заполнить количество часов, которые вы потратили на это задание
* Отправить заявку на ревью в соответствующей форме
* Изменять файлы в директории [.github](.github) запрещено
* Время для дедлайна фиксируется отправкой формы
* Запрещено отправлять форму до того, как CI успешно завершится

## Особенности реализации
* В задании нужно подумать над оптимизациями
* Используйте тесты, чтобы заполнить недосказанности

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
