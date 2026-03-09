package digest

import (
	"math"
	"math/cmplx"
	"strings"
	"unsafe"
)

// GetCharByIndex returns the i-th character from the given string.
func GetCharByIndex(str string, idx int) rune {
	if idx < 0 {
		panic("index is negative")
	}

	if len(str) == 0 {
		panic("string is empty")
	}

	cnt := 0
	for _, r := range str {
		if cnt == idx {
			return r
		}

		cnt++
	}

	panic("index greater than str length")
}

// GetStringBySliceOfIndexes returns a string formed by concatenating specific characters from the input string based
// on the provided indexes.
func GetStringBySliceOfIndexes(str string, indexes []int) string {
	if len(str) == 0 {
		panic("string is empty")
	}

	if len(indexes) == 0 {
		panic("indexes is empty")
	}

	runes := []rune(str)

	var res strings.Builder
	res.Grow(len(indexes))

	for _, i := range indexes {
		if i >= len(runes) {
			panic("index greater than str length")
		}

		if i < 0 {
			panic("index is negative")
		}

		res.WriteRune(runes[i])
	}

	return res.String()
}

// ShiftPointer shifts the given pointer by the specified number of bytes using unsafe.Add.
func ShiftPointer(pointer **int, shift int) {
	if pointer == nil {
		panic("pointer is nil")
	}

	*pointer = (*int)(unsafe.Add(unsafe.Pointer(*pointer), shift))
}

// IsComplexEqual compares two complex numbers and determines if they are equal.
func IsComplexEqual(a, b complex128) bool {
	const eps = 0.0001

	isEq := func(x, y float64) bool {
		if math.IsInf(x, 0) || math.IsInf(y, 0) {
			return math.IsInf(x, 1) && math.IsInf(y, 1) ||
				math.IsInf(x, -1) && math.IsInf(y, -1)
		}

		return math.Abs(x-y) < eps
	}

	return isEq(real(a), real(b)) && isEq(imag(a), imag(b))
}

// GetRootsOfQuadraticEquation returns two roots of a quadratic equation ax^2 + bx + c = 0.
func GetRootsOfQuadraticEquation(a, b, c float64) (complex128, complex128) {
	if a == 0 {
		if b == 0 {
			return 0, 0
		}

		x := complex(-c/b, 0)

		return x, x
	}

	d := cmplx.Sqrt(complex(b*b-4*a*c, 0))
	compx := d / complex(2*a, 0)

	base := -b / (2 * a)

	return complex(base+real(compx), imag(compx)), complex(base-real(compx), -imag(compx))
}

// Sort sorts in-place the given slice of integers in ascending order.
func Sort(source []int) {
	if len(source) < 2 {
		return
	}

	l := 0
	r := len(source) - 1
	mid := l + (r-l)/(1+(r-l)%5)
	source[mid], source[r] = source[r], source[mid]
	mid = source[r]

	i := l
	for j := l; j < r; j++ {
		if source[j] <= mid {
			source[i], source[j] = source[j], source[i]
			i++
		}
	}

	source[i], source[r] = source[r], source[i]

	Sort(source[l:i])
	Sort(source[i+1 : r+1])
}

// ReverseSliceOne in-place reverses the order of elements in the given slice.
func ReverseSliceOne(s []int) {
	if len(s) == 0 {
		return
	}

	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
}

// ReverseSliceTwo returns a new slice of integers with elements in reverse order compared to the input slice.
// The original slice remains unmodified.
func ReverseSliceTwo(s []int) []int {
	if len(s) == 0 {
		return []int{}
	}

	res := make([]int, len(s))
	for i := 0; i < len(s); i++ {
		res[i] = s[len(s)-1-i]
	}

	return res
}

// SwapPointers swaps the values of two pointers.
func SwapPointers(a, b *int) {
	if a == nil || b == nil {
		panic("one of pointers is nil")
	}

	*a, *b = *b, *a
}

// IsSliceEqual compares two slices of integers and returns true if they contain the same elements in the same order.
func IsSliceEqual(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}

	for i, el := range a {
		if el != b[i] {
			return false
		}
	}

	return true
}

// DeleteByIndex deletes the element at the specified index from the slice and returns a new slice.
// The original slice remains unmodified.
func DeleteByIndex(s []int, idx int) []int {
	if len(s) == 0 {
		panic("array is empty")
	}

	if idx < 0 {
		panic("index is negative")
	}

	if idx >= len(s) {
		panic("index greater than array length")
	}

	res := make([]int, len(s)-1)
	copy(res, s[:idx])
	copy(res[idx:], s[idx+1:])

	return res
}
