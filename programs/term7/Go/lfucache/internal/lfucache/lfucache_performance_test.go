//go:build performance_test

package lfucache

import (
	"math/rand/v2"
	"runtime"
	"runtime/debug"
	"slices"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestGetPutPerformance(t *testing.T) {
	cache := testing.Benchmark(func(b *testing.B) {
		c := New[int, int](100)

		for b.Loop() {
			c.Put(rand.N[int](10), rand.N[int](10))
			c.Get(rand.N[int](10))
		}
	})

	emulator := testing.Benchmark(func(b *testing.B) {
		a := make(map[int]int)

		for b.Loop() {
			a[rand.N[int](10)]++
			a[rand.N[int](10)]++
		}
	})

	require.LessOrEqual(t, float64(cache.NsPerOp())/float64(emulator.NsPerOp()), 14.)
}

func TestGetPutPerformanceWithHotCache(t *testing.T) {
	const count = 1000
	cache := testing.Benchmark(func(b *testing.B) {
		c := New[int, int](count)
		c.Put(-1, -1)

		for i := 1; i < count; i++ {
			for range i {
				c.Put(i, i)
			}
		}

		for b.Loop() {
			c.Put(count/2+rand.N[int](100), rand.N[int](10))
			c.Get(count/2 + rand.N[int](100))
		}
	})

	emulator := testing.Benchmark(func(b *testing.B) {
		a := make(map[int]int)
		b.ResetTimer()

		for b.Loop() {
			a[count/2+rand.N[int](100)]++
			a[count/2+rand.N[int](100)]++
		}
	})

	require.LessOrEqual(t, float64(cache.NsPerOp())/float64(emulator.NsPerOp()), 20.)
}

func TestIteratorPerformance(t *testing.T) {
	cache := testing.Benchmark(func(b *testing.B) {
		c := New[int, int](10)

		for i := 0; i < 10e7; i++ {
			c.Put(-42, -42)
		}

		for i := 1; i <= 9; i++ {
			for range i * 10_000 {
				c.Put(i, i)
			}
		}

		for b.Loop() {
			collect(c.All())
		}
	})

	emulator := testing.Benchmark(func(b *testing.B) {
		m := make([]int, 10)

		for i := 0; i < len(m); i++ {
			m[i] = i
		}

		for b.Loop() {
			collect(slices.Backward(m))
		}
	})

	require.LessOrEqual(t, float64(cache.NsPerOp())/float64(emulator.NsPerOp()), 4.)
}

func TestIteratorAllocs(t *testing.T) {
	debug.SetGCPercent(-1)
	prev := runtime.GOMAXPROCS(1)
	t.Cleanup(func() {
		debug.SetGCPercent(100)
		runtime.GOMAXPROCS(prev)
	})

	for range 5 {
		c := New[int, int](10_000)

		for i := 1; i <= 10_000; i++ {
			for range i {
				c.Put(i, i)
			}
		}

		runtime.GC()
		var stats runtime.MemStats
		runtime.ReadMemStats(&stats)
		before := stats.TotalAlloc

		c.All()(func(i int, i2 int) bool {
			return false
		})

		runtime.ReadMemStats(&stats)
		after := stats.TotalAlloc

		require.LessOrEqual(t, after-before, uint64(1<<12))
	}
}

// TestNodeReclaiming
// When deleting, the old dataEl is reused
func TestNodeReclaiming(t *testing.T) {
	debug.SetGCPercent(-1)
	prev := runtime.GOMAXPROCS(1)
	t.Cleanup(func() {
		debug.SetGCPercent(100)
		runtime.GOMAXPROCS(prev)
	})

	for range 1000 {
		cache := New[int, int](149)

		for i := 1; i <= 50; i++ {
			cache.Put(i, i)
		}

		for i := 52; i <= 150; i++ {
			for range i - 50 {
				cache.Put(i, i)
			}
		}

		runtime.GC()
		var stats runtime.MemStats
		runtime.ReadMemStats(&stats)
		before := stats.Mallocs

		for i := -50; i < -1; i++ {
			for range i * -1 {
				cache.Put(i, i)
			}
		}

		runtime.ReadMemStats(&stats)
		after := stats.Mallocs

		require.Zero(t, after-before)
	}
}

func TestPutAllocs(t *testing.T) {
	debug.SetGCPercent(-1)
	prev := runtime.GOMAXPROCS(1)

	t.Cleanup(func() {
		debug.SetGCPercent(100)
		runtime.GOMAXPROCS(prev)
	})

	for range 100 {
		cache := New[int, int](100)

		cache.Put(1, 1)
		cache.Put(2, 2)
		cache.Get(2)

		for range 10_000 {
			cache.Put(27000, 27000)
		}

		for i := 3; i <= 100; i++ {
			cache.Put(i, i)
		}

		runtime.GC()
		var stats runtime.MemStats
		runtime.ReadMemStats(&stats)
		before := stats.Mallocs

		for i := 3; i <= 100; i++ {
			cache.Put(i, i)
		}

		runtime.ReadMemStats(&stats)
		after := stats.Mallocs

		require.Zero(t, after-before)
	}
}

func TestInvalidationPerformance(t *testing.T) {
	hot := testing.Benchmark(func(b *testing.B) {
		hotCache := New[int, int](1)

		for b.Loop() {
			for range 10000 {
				hotCache.Put(42, 42)
			}

			hotCache.Put(1, 1)
			frequency, err := hotCache.GetKeyFrequency(1)
			require.NoError(t, err)
			require.Equal(t, 1, frequency)

			oldFrequency, err := hotCache.GetKeyFrequency(42)
			require.ErrorIs(t, err, ErrKeyNotFound)
			require.Zero(t, oldFrequency)
		}
	})

	cold := testing.Benchmark(func(b *testing.B) {
		coldCache := New[int, int](2)

		for b.Loop() {
			for range 10000 {
				coldCache.Put(42, 42)
			}

			coldCache.Put(42, 42)
			_, err := coldCache.GetKeyFrequency(42)
			require.NoError(t, err)

			_, err = coldCache.GetKeyFrequency(43)
			require.ErrorIs(t, err, ErrKeyNotFound)
		}
	})

	require.LessOrEqual(t, float64(hot.NsPerOp())/float64(cold.NsPerOp()), 1.3)
}
