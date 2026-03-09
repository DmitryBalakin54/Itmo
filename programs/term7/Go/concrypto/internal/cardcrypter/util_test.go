package cardcrypter

import (
	"crypto/aes"
	"crypto/cipher"
	cr "crypto/rand"
	"encoding/hex"
	mr "math/rand/v2"
	"runtime"
	"runtime/debug"
	"strconv"
	"sync"
	"sync/atomic"
	"testing"
	"time"
	"unsafe"

	"github.com/stretchr/testify/require"
)

func testCards(t *testing.T, numCards int) []Card {
	t.Helper()

	const (
		cardLen     = int(unsafe.Sizeof(CardNumber{}))
		alphabet    = "0123456789"
		alphabetLen = len(alphabet)
	)

	cards := make([]Card, numCards)
	for i := 0; i < numCards; i++ {
		b := make([]byte, cardLen)

		for j := range b {
			b[j] = alphabet[mr.N(alphabetLen)]
		}

		cards[i] = Card{
			ID:     strconv.Itoa(i),
			Number: CardNumber(b),
		}
	}

	return cards
}

func testKey(t *testing.T) []byte {
	t.Helper()
	return []byte("0123456789abcdef0123456789abcdef")
}

func decrypt(
	t *testing.T,
	ciphertexts []string,
	ids []string,
	key []byte,
) ([]CardNumber, error) {
	t.Helper()

	n := len(ciphertexts)
	require.Equal(t, len(ids), n, "ids length mismatch")
	out := make([]CardNumber, n)

	if n == 0 {
		return out, nil
	}

	block, err := aes.NewCipher(key)
	require.NoError(t, err)

	gcm, err := cipher.NewGCM(block)
	require.NoError(t, err)

	nonceSize := gcm.NonceSize()

	for i := 0; i < len(ids); i++ {
		raw, err := hex.DecodeString(ciphertexts[i])
		require.NoError(t, err)
		require.GreaterOrEqual(t, len(raw), nonceSize+gcm.Overhead(), "ciphertext too short")

		nonce := raw[:nonceSize]
		ct := raw[nonceSize:]
		aad := []byte(ids[i])

		plain, err := gcm.Open(nil, nonce, ct, aad)

		if err != nil {
			return nil, err
		}

		require.EqualValues(t, len(plain), unsafe.Sizeof(CardNumber{}), "invalid card length: %d")
		out[i] = CardNumber(plain)
	}

	return out, nil
}

func inspectMallocs(t *testing.T, f func()) int {
	debug.SetGCPercent(-1)
	t.Cleanup(func() {
		debug.SetGCPercent(100)
	})

	var result int

	for range 100 {
		runtime.GC()
		var stats runtime.MemStats
		runtime.ReadMemStats(&stats)
		before := stats.Mallocs

		f()

		runtime.ReadMemStats(&stats)
		after := stats.Mallocs

		result = max(result, int(after-before))
	}

	return result
}

func inspectNumGoroutines(t *testing.T, f func()) int {
	t.Helper()

	wg := new(sync.WaitGroup)

	result := atomic.Int64{}
	result.Store(int64(runtime.NumGoroutine()))

	done := atomic.Bool{}
	wg.Go(func() {
		f()
		done.Store(true)
	})

	wg.Go(func() {
		for !done.Load() {
			result.Store(max(result.Load(), int64(runtime.NumGoroutine())))
		}
	})

	wg.Wait()
	return int(result.Load()) - 2 - 3
}

func forceMockReader(t *testing.T) *testRandReader {
	prev := cr.Reader
	t.Cleanup(func() {
		cr.Reader = prev
	})

	testReader := newTestReader(t)
	cr.Reader = testReader

	return testReader
}

func mockReader(t *testing.T) {
	if runtime.GOOS == "linux" {
		return
	}

	prev := cr.Reader
	t.Cleanup(func() {
		cr.Reader = prev
	})

	cr.Reader = newTestReader(t)
}

func mockReaderWithConstant(t *testing.T) {
	r := forceMockReader(t)
	r.deterministic = true
}

func mockReaderWithTimeout(t *testing.T, timeout time.Duration) *testRandReader {
	r := forceMockReader(t)
	r.sleepTime = timeout
	r.sleep = true

	return r
}

type testRandReader struct {
	mx *sync.Mutex

	random *mr.ChaCha8
	t      *testing.T
	calls  atomic.Int64

	sleepTime time.Duration
	sleep     bool

	deterministic bool
}

func newTestReader(t *testing.T) *testRandReader {
	return &testRandReader{
		mx:     new(sync.Mutex),
		t:      t,
		random: mr.NewChaCha8([32]byte([]byte("ABCDEFGHIJKLMNOPQRSTUVWXYZ123456"))),
	}
}

func (r *testRandReader) Read(p []byte) (n int, err error) {
	// On macOS and OpenBSD, crypto/rand relies on arc4random,
	// which uses a spin lock and blocks on every call.
	// This causes severe performance degradation under multi-threaded workloads.
	// https://github.com/golang/go/blob/c58d075e9a457fce92bdf60e2d1870c8c4df7dc5/src/internal/syscall/unix/arc4random_darwin.go#L22

	// To avoid this bottleneck, we mock the crypto random source
	// on these platforms, providing stable performance without
	// lock contention.

	if r.sleep {
		time.Sleep(r.sleepTime)
	}

	r.calls.Add(1)

	if r.deterministic {
		for i := 0; i < len(p); i++ {
			p[i] = '1'
		}

		return len(p), nil
	}

	for len(p) >= 8 {
		LEPutUint64(p, r.random.Uint64())
		p = p[8:]
		n += 8
	}

	v := r.random.Uint64()
	for i := 0; i < len(p); i++ {
		p[i] = byte(v >> (8 * v))
		n++
	}

	return n, nil
}

func LEPutUint64(b []byte, v uint64) {
	_ = b[7]
	b[0] = byte(v)
	b[1] = byte(v >> 8)
	b[2] = byte(v >> 16)
	b[3] = byte(v >> 24)
	b[4] = byte(v >> 32)
	b[5] = byte(v >> 40)
	b[6] = byte(v >> 48)
	b[7] = byte(v >> 56)
}
