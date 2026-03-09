//go:build model_test

package lfucache

import (
	"fmt"
	"math/rand/v2"
	"slices"
	"testing"
	"unsafe"

	"github.com/stretchr/testify/require"
)

// must compile
func testImplements[K comparable, V any]() Cache[K, V] {
	return New[K, V](1)
}

func TestWithoutInvalidation(t *testing.T) {
	t.Parallel()

	cache := New[int, int](3)
	require.Equal(t, unsafe.Sizeof((*int)(nil)), unsafe.Sizeof(cache))

	cache.Put(1, 1)
	cache.Put(2, 4)
	cache.Put(3, 9)

	value, err := cache.Get(1)
	require.NoError(t, err)
	require.Equal(t, 1, value)

	value, err = cache.Get(2)
	require.NoError(t, err)
	require.Equal(t, 4, value)

	value, err = cache.Get(3)
	require.NoError(t, err)
	require.Equal(t, 9, value)

	_, err = cache.Get(1)
	require.NoError(t, err)

	frequency, err := cache.GetKeyFrequency(1)
	require.NoError(t, err)
	require.Equal(t, 3, frequency)

	keys, values := collect(cache.All())
	require.Equal(t, []int{1, 3, 2}, keys)
	require.Equal(t, []int{1, 9, 4}, values)
}

func TestIteratorOrder(t *testing.T) {
	cache := New[int, int](100)

	for i := 0; i < 1234; i++ {
		cache.Put(i%(rand.N[int](5)+1), rand.N(1000))
	}

	keys, values := collect(cache.All())
	frequencyList := make([]int, 0, len(keys))

	for i, k := range keys {
		v, err := cache.Get(k)
		require.NoError(t, err)
		require.Equal(t, values[i], v)

		freq, err := cache.GetKeyFrequency(k)
		require.NoError(t, err)

		frequencyList = append(frequencyList, freq)
	}

	fmt.Println(frequencyList)
	require.True(t, slices.IsSortedFunc(frequencyList, func(a, b int) int {
		if a >= b {
			fmt.Println(a, " ", b)
			return -1
		}

		return 1
	}))
}

func TestIteratorDifferentFrequency(t *testing.T) {
	t.Parallel()

	cache := New[int, int](5)

	cache.Put(1, 10)
	cache.Put(2, 20)
	cache.Put(3, 30)
	cache.Put(4, 40)
	cache.Put(5, 50)

	for i := 1; i <= 5; i++ {
		for range i {
			_, _ = cache.Get(i)
		}
	}

	iterator := cache.All()
	keys := make([]int, 0, 2)
	values := make([]int, 0, 2)

	iterator(func(k int, v int) bool {
		if k == 3 && v == 30 {
			return false
		}

		keys = append(keys, k)
		values = append(values, v)

		return true
	})

	require.Equal(t, []int{5, 4}, keys)
	require.Equal(t, []int{50, 40}, values)
}

func TestKeyNotFound(t *testing.T) {
	t.Parallel()

	cache := New[int, int](3)

	_, err := cache.Get(1)
	require.ErrorIs(t, err, ErrKeyNotFound)
}

func TestUpdatePutFrequency(t *testing.T) {
	t.Parallel()

	cache := New[int, int](3)

	cache.Put(1, 10)
	cache.Put(2, 20)
	cache.Put(3, 30)

	cache.Put(3, 30)
	cache.Put(2, 20)
	cache.Put(1, 10)

	v1, err := cache.GetKeyFrequency(1)
	require.NoError(t, err)
	require.Equal(t, 2, v1)

	v2, err := cache.GetKeyFrequency(2)
	require.Equal(t, 2, v2)
	require.NoError(t, err)

	v3, err := cache.GetKeyFrequency(3)
	require.Equal(t, 2, v3)
	require.NoError(t, err)

	keys, values := collect(cache.All())

	require.Equal(t, []int{1, 2, 3}, keys)
	require.Equal(t, []int{10, 20, 30}, values)
}

func TestDefaultCapacity(t *testing.T) {
	t.Parallel()

	cache := New[***int, ***int]()
	require.Equal(t, DefaultCapacity, cache.Capacity())
}

func TestIterator(t *testing.T) {
	t.Parallel()

	cache := New[int, int](4)

	cache.Put(1, 10)
	cache.Put(2, 20)
	cache.Put(3, 30)
	cache.Put(4, 40)
	cache.Put(5, 50)

	iterator := cache.All()
	keys := make([]int, 0, 4)
	values := make([]int, 0, 4)

	iterator(func(k int, v int) bool {
		if k == 2 && v == 20 {
			return false
		}

		keys = append(keys, k)
		values = append(values, v)

		return true
	})

	require.Equal(t, []int{5, 4, 3}, keys)
	require.Equal(t, []int{50, 40, 30}, values)
}

func TestFrequencyReplacement(t *testing.T) {
	t.Parallel()

	cache := New[int, int](2)
	cache.Put(1, 10)
	cache.Put(2, 20)

	value, err := cache.Get(1)
	require.NoError(t, err)
	require.Equal(t, 10, value)

	cache.Put(3, 30)

	_, err = cache.Get(2)
	require.Equal(t, ErrKeyNotFound, err)

	value, err = cache.Get(3)
	require.NoError(t, err)
	require.Equal(t, 30, value)

	cache.Put(4, 40)

	_, err = cache.Get(1)
	require.ErrorIs(t, err, ErrKeyNotFound)

	value, err = cache.Get(3)
	require.NoError(t, err)
	require.Equal(t, 30, value)

	value, err = cache.Get(4)
	require.NoError(t, err)
	require.Equal(t, 40, value)

	keys, values := collect(cache.All())

	require.Equal(t, []int{3, 4}, keys)
	require.Equal(t, []int{30, 40}, values)
}

func TestCacheSize(t *testing.T) {
	t.Parallel()

	cache := New[int, int](1)

	cache.Put(1, 10)
	require.Equal(t, 1, cache.Size())
}

func TestNegativeCapacityPanics(t *testing.T) {
	t.Parallel()

	require.Panics(t, func() {
		New[int, int](-1)
	})
}

func TestGetKeyFrequencyNonExistent(t *testing.T) {
	t.Parallel()

	cache := New[int, int](0)

	_, err := cache.GetKeyFrequency(1)
	require.ErrorIs(t, err, ErrKeyNotFound)
}

func TestGetIncreasesFrequency(t *testing.T) {
	t.Parallel()

	cache := New[*int, string](1)
	key := new(int)

	cache.Put(key, "zero")
	_, _ = cache.Get(key)
	_, _ = cache.Get(key)

	freq, err := cache.GetKeyFrequency(key)
	require.NoError(t, err)
	require.Equal(t, 3, freq)
}

func TestUpdateValueChangeFrequency(t *testing.T) {
	t.Parallel()

	cache := New[int, string](2)

	cache.Put(1, "one")
	_, _ = cache.Get(1)

	cache.Put(1, "first")

	value, err := cache.Get(1)
	require.NoError(t, err)
	require.Equal(t, "first", value)

	freq, err := cache.GetKeyFrequency(1)
	require.NoError(t, err)
	require.Equal(t, 4, freq)
}

func TestAllOrdering(t *testing.T) {
	t.Parallel()

	cache := New[int, int](3)

	cache.Put(1, 10)
	cache.Put(2, 20)
	cache.Put(3, 30)

	_, _ = cache.Get(2)
	_, _ = cache.Get(3)
	_, _ = cache.Get(3)

	keys, values := collect(cache.All())

	require.Equal(t, []int{3, 2, 1}, keys)
	require.Equal(t, []int{30, 20, 10}, values)
}

func TestWithCustomTypes(t *testing.T) {
	t.Parallel()

	type myKey struct {
		id int
	}

	type myValue struct {
		name string
	}

	cache := New[myKey, myValue](1)

	k1 := myKey{id: 1}
	v1 := myValue{name: "one"}

	k2 := myKey{id: 2}
	v2 := myValue{name: "two"}

	cache.Put(k1, v1)
	cache.Put(k2, v2)

	_, err := cache.Get(k1)
	require.ErrorIs(t, err, ErrKeyNotFound)

	value, err := cache.Get(k2)
	require.NoError(t, err)
	require.Equal(t, v2, value)
}

func TestAllOnEmptyCache(t *testing.T) {
	t.Parallel()

	cache := New[int, int](1)
	keys, values := collect(cache.All())

	require.Empty(t, keys)
	require.Empty(t, values)
}

func TestEvictionTieBreaker(t *testing.T) {
	t.Parallel()

	cache := New[int, string](2)

	cache.Put(1, "one")
	cache.Put(2, "two")
	cache.Put(3, "three")

	_, err := cache.Get(1)
	require.ErrorIs(t, err, ErrKeyNotFound)

	value, err := cache.Get(2)
	require.NoError(t, err)
	require.Equal(t, "two", value)

	value, err = cache.Get(3)
	require.NoError(t, err)
	require.Equal(t, "three", value)
}

func TestAllIterator(t *testing.T) {
	t.Parallel()

	cache := New[int, int](5)

	cache.Put(1, 10)
	cache.Put(2, 20)
	cache.Put(3, 30)
	cache.Put(4, 40)
	cache.Put(5, 50)

	keys, values := collect(cache.All())

	require.Equal(t, []int{5, 4, 3, 2, 1}, keys)
	require.Equal(t, []int{50, 40, 30, 20, 10}, values)
}
