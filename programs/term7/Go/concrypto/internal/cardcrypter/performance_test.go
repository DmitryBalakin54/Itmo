//go:build performance_test

package cardcrypter

import (
	"fmt"
	"runtime"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestEncryptPerformance(t *testing.T) {
	mockReaderWithTimeout(t, time.Nanosecond)

	singleThread := testing.Benchmark(func(b *testing.B) {
		b.Run("1 thread", func(b *testing.B) {
			b.ReportAllocs()

			crypter := New(WithWorkers(1))
			key := testKey(t)
			cards := testCards(t, 10_000)

			for b.Loop() {
				_, err := crypter.Encrypt(cards, key)
				require.NoError(b, err)
			}
		})
	})

	workers := runtime.GOMAXPROCS(-1)
	nThread := testing.Benchmark(func(b *testing.B) {
		b.Run("n threads", func(b *testing.B) {
			b.ReportAllocs()

			crypter := New(WithWorkers(workers))
			key := testKey(t)
			cards := testCards(t, 10_000)

			for b.Loop() {
				_, err := crypter.Encrypt(cards, key)
				require.NoError(b, err)
			}
		})
	})

	expected := getExpectedSpeed(workers)
	fmt.Println("expected: ", expected)

	actual := float64(singleThread.NsPerOp()) / float64(nThread.NsPerOp())
	fmt.Println("actual: ", actual)

	fmt.Println("workers: ", runtime.GOMAXPROCS(-1))
	require.GreaterOrEqual(t, actual, expected, "too slow")
}

func TestZeroSliceMallocs(t *testing.T) {
	key := testKey(t)
	cards := testCards(t, 0)

	crypter := New()
	mallocs := inspectMallocs(t, func() {
		crypter.Encrypt(cards, key)
	})

	require.LessOrEqual(t, mallocs, 1)
}

func TestMallocs(t *testing.T) {
	key := testKey(t)
	cards := testCards(t, 100)

	crypter := New(WithWorkers(1))
	mallocs := inspectMallocs(t, func() {
		crypter.Encrypt(cards, key)
	})

	// try to optimize []byte <-> string conversion
	// unsafe.Slice(), unsafe.SliceData(), unsafe.String(), unsafe.StringData()

	// Also try to check hex implementation.
	// Think about local variables too.

	require.LessOrEqual(t, mallocs/len(cards), 1) // ~1 + eps
}

func TestWorkersDistribution(t *testing.T) {
	mockReaderWithTimeout(t, time.Second)

	const (
		cardsNum = 990
		workers  = 500
	)

	key := testKey(t)
	cards := testCards(t, cardsNum)
	crypter := New(WithWorkers(workers))

	start := time.Now()
	_, err := crypter.Encrypt(cards, key)
	require.NoError(t, err)
	end := time.Now()

	require.Equal(t, 2, int(end.Sub(start).Seconds()))
}

func getExpectedSpeed(workers int) float64 {
	const d = 0.3
	return 1 / (d + (1-d)/float64(workers))
}
