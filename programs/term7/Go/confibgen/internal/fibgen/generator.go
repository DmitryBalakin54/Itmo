package fibonacci

import (
	"math"
	"sync/atomic"
)

type overflowError struct{}

func (overflowError) Error() string {
	return "fibonacci overflow: next value would overflow uint64"
}

type Generator interface {
	Next() uint64
}

var _ Generator = (*generatorImpl)(nil)

type generatorImpl struct {
	index atomic.Uint32
	//v     atomic.Bool
	//a     uint64
	//b     uint64
	//i     uint32
}

func NewGenerator() *generatorImpl {
	return &generatorImpl{}
}

func (g *generatorImpl) Next() uint64 {
	//for !g.v.CompareAndSwap(false, true) {
	//}
	//
	//defer func() {
	//	g.i++
	//	g.v.Store(false)
	//}()
	//
	//index := g.i
	//if index == 0 {
	//	return g.b
	//}
	//
	//if index == 1 {
	//	g.b++
	//	return g.b
	//}
	//
	//g.a, g.b = g.b, add(g.a, g.b)
	//return g.b

	return fib(g.index.Add(1) - 1)
}

func f(n uint32) uint64 {
	// return uint64(math.Round(math.Pow(math.Phi, float64(n)) / math.Sqrt(5))) magic number :(
	return uint64(math.Round((math.Pow(math.Phi, float64(n)) - math.Pow(-math.Phi, -float64(n))) / (2*math.Phi - 1)))
}

func add(a, b uint64) uint64 {
	if a > math.MaxUint64-b {
		panic(new(overflowError))
	}

	return a + b
}

func mul(a, b uint64) uint64 {
	if a > math.MaxUint64/b {
		panic(new(overflowError))
	}

	return a * b
}

func fib(n uint32) uint64 {
	if n == 0 {
		return 0
	}

	if n == 1 {
		return 1
	}

	prev := n / 2

	f0 := f(prev)
	f1 := f(prev + 1)

	if n%2 == 0 {
		return mul(f0, mul(f1, 2)-f0)
	}

	return add(mul(f0, f0), mul(f1, f1))
}
