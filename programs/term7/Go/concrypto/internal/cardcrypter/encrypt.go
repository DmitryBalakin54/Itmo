package cardcrypter

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"runtime"
	"sync"
	"sync/atomic"
	"unsafe"
)

const maxWorkers = 1024

type CardNumber [16]byte

type Card struct {
	ID     string
	Number CardNumber
}

type Crypter interface {
	Encrypt(cards []Card, key []byte) ([]string, error)
}

type crypterImpl struct {
	workers int
}

func New(opts ...CrypterOption) *crypterImpl {
	impl := &crypterImpl{runtime.GOMAXPROCS(0)}

	for _, opt := range opts {
		opt(impl)
	}

	return impl
}

type CrypterOption func(*crypterImpl)

func WithWorkers(workers int) CrypterOption {
	return func(impl *crypterImpl) {
		impl.workers = workers
	}
}

func (c *crypterImpl) Encrypt(cards []Card, key []byte) ([]string, error) {
	n := len(cards)
	if n == 0 {
		return nil, nil
	}

	workers := c.workers
	if workers < 0 {
		return nil, fmt.Errorf("invalid number of workers(negative workers): %d", workers)
	}

	if workers == 0 {
		return nil, fmt.Errorf("invalid number of workers(no workers): %d", workers)
	}

	bound := min(n, maxWorkers)
	if workers > bound {
		workers = bound
	}

	wg := new(sync.WaitGroup)
	startWG := new(sync.WaitGroup)
	workerErr := new(atomic.Value)
	ind := new(atomic.Int64)

	block, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}

	gcm, err := cipher.NewGCM(block)
	if err != nil {
		return nil, err
	}

	nonceSize := gcm.NonceSize()
	lenOfOne := (nonceSize + 16 + gcm.Overhead()) * 2
	buffer := make([]byte, n*lenOfOne)

	out := make([]string, n)
	for i := 0; i < n; i++ {
		start := i * lenOfOne
		out[i] = unsafe.String(&buffer[start], lenOfOne)
	}

	wg.Add(workers)
	startWG.Add(workers)

	for w := 0; w < workers; w++ {
		wg.Go(func() {
			defer wg.Done()

			startWG.Done()
			startWG.Wait()

			for {
				if workerErr.Load() != nil {
					return
				}

				i := int(ind.Add(1) - 1)
				if i >= n {
					return
				}

				nonce := unsafe.Slice(unsafe.StringData(out[i]), nonceSize)
				if _, err := rand.Read(nonce); err != nil {
					workerErr.Store(err)
					return
				}

				encrypted, err := safeSeal(
					gcm,
					nonce,
					cards[i].Number[:],
					unsafe.Slice(unsafe.StringData(cards[i].ID), len(cards[i].ID)),
					nonce,
				)
				if err != nil {
					workerErr.Store(err)
					return
				}

				hex.Encode(unsafe.Slice(unsafe.StringData(out[i]), len(out[i])), encrypted)
			}
		})
	}

	wg.Wait()

	if err := workerErr.Load(); err != nil {
		return nil, err.(error)
	}

	return out, nil
}

func safeSeal(gcm cipher.AEAD, nonce, plaintext, aad []byte, dst []byte) (res []byte, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()

	res = gcm.Seal(dst, nonce, plaintext, aad)

	return
}
