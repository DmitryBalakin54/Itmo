//go:build performance_test

package fact

import (
	"context"
	"runtime"
	"runtime/debug"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestFallbackPerformance(t *testing.T) {
	deferrableLeakDetection(t)

	first := testing.Benchmark(func(b *testing.B) {
		ctx := context.Background()

		fact := newFactorizer(t, runtime.GOMAXPROCS(-1), runtime.GOMAXPROCS(-1))
		numbers := generateNumbers(10)

		for b.Loop() {
			err := fact.Factorize(ctx, numbers, newWriter())
			require.NoError(t, err)
		}
	})

	second := testing.Benchmark(func(b *testing.B) {
		ctx := context.Background()

		fact, err := New()
		require.NoError(t, err)

		numbers := generateNumbers(10)

		for b.Loop() {
			err = fact.Factorize(ctx, numbers, newWriter())
			require.NoError(t, err)
		}
	})

	require.LessOrEqual(t, float64(second.NsPerOp())/float64(first.NsPerOp()), 1.03)
}

func TestGeneralPerformance(t *testing.T) {
	deferrableLeakDetection(t)

	first := testing.Benchmark(func(b *testing.B) {
		ctx := context.Background()

		fact := newFactorizer(t, max(2, runtime.NumCPU()), max(4, runtime.NumCPU()))
		numbers := generateNumbers(100)

		for b.Loop() {
			err := fact.Factorize(ctx, numbers, newSleepWriter(time.Millisecond*100))
			require.NoError(t, err)
		}
	})

	second := testing.Benchmark(func(b *testing.B) {
		ctx := context.Background()

		fact := newFactorizer(t, max(2, runtime.NumCPU())/2, max(4, runtime.NumCPU())/4)
		numbers := generateNumbers(100)

		for b.Loop() {
			err := fact.Factorize(ctx, numbers, newSleepWriter(time.Millisecond*100))
			require.NoError(t, err)
		}
	})

	require.GreaterOrEqual(t, float64(second.NsPerOp())/float64(first.NsPerOp()), 3.5)
}

func TestWorkersCount(t *testing.T) {
	deferrableLeakDetection(t)

	ctx := context.Background()

	const (
		factWorkers  = 50
		writeWorkers = 1000
	)

	fact := newFactorizer(t, factWorkers, writeWorkers)
	numbers := generateNumbers(1000)

	debug.SetGCPercent(-1)
	t.Cleanup(func() {
		debug.SetGCPercent(100)
	})

	gNum := inspectNumGoroutines(t, func() {
		err := fact.Factorize(ctx, numbers, newSleepWriter(time.Second))
		require.NoError(t, err)
	})

	require.InDelta(t, gNum, factWorkers+writeWorkers, 50)
}
