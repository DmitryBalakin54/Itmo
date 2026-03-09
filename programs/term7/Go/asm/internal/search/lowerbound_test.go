package search

import (
	"slices"
	"testing"

	"github.com/stretchr/testify/require"
)

func LowerBound(slice []int64, value int64) int64

func TestLowerBound(t *testing.T) {
	t.Parallel()

	type testCases struct {
		name   string
		arg    []int64
		value  int64
		result int64
	}

	tableTests := []testCases{
		{
			name:   "exact match",
			arg:    []int64{1, 2, 3, 4},
			value:  3,
			result: 2,
		},
		{
			name:   "empty",
			arg:    []int64{},
			value:  100,
			result: 0,
		},
		{
			name:   "none",
			arg:    []int64{10, 20, 30},
			value:  5,
			result: 0,
		},
		{
			name:   "last",
			arg:    []int64{5, 6, 7},
			value:  11,
			result: 3,
		},
		{
			name:   "one",
			arg:    []int64{-1},
			value:  -1,
			result: 0,
		},
		{
			name:   "one",
			arg:    []int64{-1},
			value:  1,
			result: 1,
		},
		{
			name:   "first",
			arg:    []int64{5, 10, 15},
			value:  7,
			result: 1,
		},
		{
			name:   "lower match",
			arg:    []int64{1, 2, 6, 8},
			value:  7,
			result: 3,
		},
		{
			name:   "multiple repeating",
			arg:    []int64{1, 2, 2, 2, 3, 4},
			value:  2,
			result: 1,
		},
	}

	for _, tt := range tableTests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			require.Equal(t, tt.result, LowerBound(tt.arg, tt.value))
		})
	}

	t.Run("32-overflow", func(t *testing.T) {
		t.Parallel()

		s := make([]int64, 10)

		s[len(s)-5] = 1 << 33
		s[len(s)-4] = 1 << 34
		s[len(s)-3] = 1 << 35
		s[len(s)-2] = 1 << 36
		s[len(s)-1] = 1 << 37

		require.EqualValues(t, len(s)-3, LowerBound(s, 1<<35))
	})
}

func TestPerformance(t *testing.T) {
	const index = 432000

	solution := testing.Benchmark(func(b *testing.B) {
		s := make([]int64, 1_000_000)

		for i := 0; i < len(s); i++ {
			s[i] = int64(i)
		}

		b.ResetTimer()

		for i := 0; i < b.N; i++ {
			ind := LowerBound(s, int64(index))
			require.Equal(t, int64(index), ind)
		}
	})

	check := testing.Benchmark(func(b *testing.B) {
		s := make([]int64, 1_000_000)

		for i := 0; i < len(s); i++ {
			s[i] = int64(i)
		}

		b.ResetTimer()

		for i := 0; i < b.N; i++ {
			ind, ok := slices.BinarySearch(s, index)

			require.True(t, ok)
			require.Equal(t, index, ind)
		}
	})

	require.Less(t, (float64(solution.NsPerOp())+10e-9)/(float64(check.NsPerOp())+10e-9), 3.0)
}

func TestLowerBoundAdvanced(t *testing.T) {
	t.Parallel()

	type testCase struct {
		name   string
		arg    []int64
		value  int64
		result int64
	}

	tests := []testCase{
		{
			name:   "all equal == value (leftmost)",
			arg:    []int64{5, 5, 5, 5},
			value:  5,
			result: 0,
		},
		{
			name:   "all equal < value -> end",
			arg:    []int64{3, 3, 3},
			value:  4,
			result: 3,
		},
		{
			name:   "all equal > value -> 0",
			arg:    []int64{9, 9, 9, 9},
			value:  1,
			result: 0,
		},
		{
			name:   "duplicates at start (leftmost among equals)",
			arg:    []int64{2, 2, 2, 3, 4},
			value:  2,
			result: 0,
		},
		{
			name:   "duplicates at end (first >= target at tail)",
			arg:    []int64{1, 2, 3, 4, 4, 4},
			value:  4,
			result: 3,
		},
		{
			name:   "negative values exact match (leftmost)",
			arg:    []int64{-10, -5, -5, -2, 0, 7},
			value:  -5,
			result: 1,
		},
		{
			name:   "negative values insert position",
			arg:    []int64{-10, -6, -3, 1, 8},
			value:  -4,
			result: 2,
		},
		{
			name:   "insert at beginning",
			arg:    []int64{10, 20, 30},
			value:  1,
			result: 0,
		},
		{
			name:   "insert in middle (gap)",
			arg:    []int64{1, 4, 7, 10},
			value:  6,
			result: 2,
		},
		{
			name:   "insert at end (all < value)",
			arg:    []int64{1, 4, 7, 10},
			value:  99,
			result: 4,
		},
		{
			name:   "first greater than value",
			arg:    []int64{5, 6, 7},
			value:  5,
			result: 0,
		},
		{
			name:   "value between equal blocks",
			arg:    []int64{1, 1, 1, 3, 3, 3, 5, 5},
			value:  4,
			result: 6,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			require.Equal(t, tt.result, LowerBound(tt.arg, tt.value))
		})
	}
}
