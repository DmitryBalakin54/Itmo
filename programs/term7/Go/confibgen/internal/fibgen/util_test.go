package fibonacci

import (
	"runtime"
	"runtime/debug"
	"sync"
	"sync/atomic"
	"testing"
)

const (
	maxFibonacciNumber             = 94
	maxFibonacciNumberValue uint64 = 12200160415121876738
)

func isFibonacci(t *testing.T, seq []uint64) bool {
	t.Helper()

	n := len(seq)

	if n == 0 {
		return false
	}

	if seq[0] != 0 {
		return false
	}

	if n == 1 {
		return true
	}

	if seq[1] != 1 {
		return false
	}

	for i := 2; i < n; i++ {
		if seq[i] != seq[i-1]+seq[i-2] {
			return false
		}
	}

	return true
}

func inspectMallocs(t *testing.T, f func()) int {
	t.Helper()

	debug.SetGCPercent(-1)
	t.Cleanup(func() {
		debug.SetGCPercent(100)
	})

	var result int

	for range maxFibonacciNumber {
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
	return int(result.Load()) - 2
}
