//go:build performance_test

package fibonacci

import (
	"runtime"
	"runtime/debug"
	"sync/atomic"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestPerformance(t *testing.T) {
	solution := testing.Benchmark(func(b *testing.B) {
		b.ReportAllocs()

		generator := NewGenerator()
		counter := 0

		for b.Loop() {
			if counter == maxFibonacciNumber {
				b.StopTimer()
				generator = NewGenerator()
				counter = 0
				b.StartTimer()
			}

			generator.Next()
			counter++
			time.Sleep(time.Millisecond)
		}
	})

	emulation := testing.Benchmark(func(b *testing.B) {
		b.ReportAllocs()

		counter := 0

		var prev, cur uint64 = 0, 1
		for b.Loop() {
			if counter == maxFibonacciNumber {
				b.StopTimer()
				counter = 0
				prev, cur = 0, 1
				b.StartTimer()
			}

			tmp := prev
			atomic.CompareAndSwapUint64(&prev, prev, cur)
			cur += tmp

			counter++
			time.Sleep(time.Millisecond)
		}
	})

	actual := float64(solution.NsPerOp()) / float64(emulation.NsPerOp())
	require.LessOrEqual(t, actual, 1.05)
}

func TestMallocs(t *testing.T) {
	generator := NewGenerator()
	mallocs := inspectMallocs(t, func() {
		generator.Next()
	})

	require.Zero(t, mallocs, "expected zero allocations on Next call")
}

func TestMallocsWithOverflow(t *testing.T) {
	debug.SetGCPercent(-1)
	t.Cleanup(func() {
		debug.SetGCPercent(100)
	})

	var result int
	generator := NewGenerator()

	for range maxFibonacciNumber {
		runtime.GC()
		var stats runtime.MemStats
		runtime.ReadMemStats(&stats)
		before := stats.Mallocs

		generator.Next()

		runtime.ReadMemStats(&stats)
		after := stats.Mallocs

		result = max(result, int(after-before))
	}

	var stats runtime.MemStats
	runtime.ReadMemStats(&stats)
	before := stats.Mallocs

	defer func() {
		if err := recover(); err == nil {
			t.Fail()
		}

		runtime.ReadMemStats(&stats)
		after := stats.Mallocs

		result = max(result, int(after-before))
		require.Zero(t, result, "expected zero allocations on Next call")
	}()

	generator.Next()
}
