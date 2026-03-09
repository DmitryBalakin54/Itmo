//go:build model_test

package fibonacci

import (
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"unsafe"

	"github.com/stretchr/testify/require"
)

func TestGoldenSequence(t *testing.T) {
	t.Parallel()

	generator := NewGenerator()
	fibSequence := make([]uint64, 0, maxFibonacciNumber)

	// https://tab.wikimassa.org/tablitsa_chisel_fibonachchi_200?i=1
	expected := [maxFibonacciNumber]uint64{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817, 39088169, 63245986, 102334155, 165580141, 267914296, 433494437, 701408733, 1134903170, 1836311903, 2971215073, 4807526976, 7778742049, 12586269025, 20365011074, 32951280099, 53316291173, 86267571272, 139583862445, 225851433717, 365435296162, 591286729879, 956722026041, 1548008755920, 2504730781961, 4052739537881, 6557470319842, 10610209857723, 17167680177565, 27777890035288, 44945570212853, 72723460248141, 117669030460994, 190392490709135, 308061521170129, 498454011879264, 806515533049393, 1304969544928657, 2111485077978050, 3416454622906707, 5527939700884757, 8944394323791464, 14472334024676221, 23416728348467685, 37889062373143906, 61305790721611591, 99194853094755497, 160500643816367088, 259695496911122585, 420196140727489673, 679891637638612258, 1100087778366101931, 1779979416004714189, 2880067194370816120, 4660046610375530309, 7540113804746346429, 12200160415121876738}

	for range maxFibonacciNumber {
		fibSequence = append(fibSequence, generator.Next())
	}

	require.Equal(t, expected[:], fibSequence)
}

func TestOverflow(t *testing.T) {
	t.Parallel()
	generator := NewGenerator()

	var prev uint64
	for range maxFibonacciNumber {
		cur := generator.Next()
		require.LessOrEqual(t, prev, cur)

		prev = cur
	}
}

func TestMaxGoldenValue(t *testing.T) {
	t.Parallel()

	generator := NewGenerator()
	var cur uint64

	for range maxFibonacciNumber {
		cur = generator.Next()
	}

	require.EqualValues(t, maxFibonacciNumberValue, cur)
}

func TestPanicOnOverflow(t *testing.T) {
	t.Parallel()
	generator := NewGenerator()

	var prev uint64
	for range maxFibonacciNumber {
		cur := generator.Next()
		require.LessOrEqual(t, prev, cur)

		prev = cur
	}

	func() {
		defer func() {
			err := recover()
			require.NotNil(t, err, "expected panic on overflow")

			vErr, ok := err.(error)
			require.True(t, ok, "expected panic with error on overflow")

			require.ErrorContains(t, vErr, "overflow", "expected verbose message on overflow")
		}()

		generator.Next()
	}()
}

func TestFibonacciInvariant(t *testing.T) {
	t.Parallel()

	for range 10000 {
		workers := 10
		generator := NewGenerator()
		const iters = 9

		wg := new(sync.WaitGroup)
		workerValues := make([][]uint64, workers)
		for workerID := range workers {
			workerValues[workerID] = make([]uint64, 0, iters)
		}

		for workerID := range workers {
			wg.Go(func() {
				for range iters {
					value := generator.Next()
					workerValues[workerID] = append(workerValues[workerID], value)
				}
			})
		}

		wg.Wait()

		allValues := make([]uint64, 0, iters*workers)
		for _, values := range workerValues {
			require.Truef(t, slices.IsSorted(values), "expected increasing sequence of values, got: %v", values)
			allValues = append(allValues, values...)
		}

		slices.Sort(allValues)
		require.True(t, isFibonacci(t, allValues), "expected fibonacci sequence")
	}
}

func TestUnlockPanicOnOverflow(t *testing.T) {
	t.Parallel()
	generator := NewGenerator()

	var prev uint64
	for range maxFibonacciNumber {
		cur := generator.Next()
		require.LessOrEqual(t, prev, cur)

		prev = cur
	}

	wg := new(sync.WaitGroup)
	for range 100 {
		wg.Go(func() {
			func() {
				defer func() {
					err := recover()
					require.NotNil(t, err, "expected panic on overflow")

					vErr, ok := err.(error)
					require.True(t, ok, "expected panic with error on overflow")

					require.ErrorContains(t, vErr, "overflow", "expected verbose message on overflow")
				}()

				generator.Next()
			}()
		})
	}

	wg.Wait()
}

func TestInternalSize(t *testing.T) {
	require.LessOrEqual(t, unsafe.Sizeof(generatorImpl{}), unsafe.Sizeof(int64(0))*4)
}

func TestNoInitFunc(t *testing.T) {
	t.Parallel()

	filesToCheck := []string{
		"./generator.go",
	}

	for _, relPath := range filesToCheck {
		absPath, err := filepath.Abs(relPath)
		require.NoError(t, err)

		fset := token.NewFileSet()
		node, err := parser.ParseFile(fset, absPath, nil, parser.AllErrors)
		require.NoError(t, err)

		for _, decl := range node.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok {
				continue
			}

			if fn.Name != nil && fn.Name.Name == "init" && fn.Recv == nil {
				require.Failf(t, "init used",
					"File %s has init function at position %v",
					relPath, fset.Position(fn.Pos()))
			}
		}
	}
}

func TestNoGlobalVars(t *testing.T) {
	t.Parallel()

	filesToCheck := []string{
		"./generator.go",
	}

	for _, relPath := range filesToCheck {
		absPath, err := filepath.Abs(relPath)
		require.NoError(t, err)

		fset := token.NewFileSet()
		node, err := parser.ParseFile(fset, absPath, nil, parser.AllErrors)
		require.NoError(t, err)

		for _, decl := range node.Decls {
			gen, ok := decl.(*ast.GenDecl)
			if !ok || gen.Tok != token.VAR {
				continue
			}

			for _, spec := range gen.Specs {
				vs, ok := spec.(*ast.ValueSpec)
				if !ok {
					continue
				}

				// var ErrX = errors.New("...")
				if len(vs.Names) == 1 &&
					strings.HasPrefix(vs.Names[0].Name, "Err") &&
					len(vs.Values) == 1 {

					// errors.New("...")
					if call, ok := vs.Values[0].(*ast.CallExpr); ok {
						if fun, ok := call.Fun.(*ast.SelectorExpr); ok {
							if pkg, ok := fun.X.(*ast.Ident); ok &&
								pkg.Name == "errors" &&
								fun.Sel.Name == "New" {
								continue // ok
							}
						}
					}
				}

				// var _ Interface = (*Type)(nil)
				if len(vs.Names) == 1 &&
					vs.Names[0].Name == "_" &&
					len(vs.Values) == 1 {

					if call, ok := vs.Values[0].(*ast.CallExpr); ok {
						// (*T)(nil)
						if _, ok := call.Fun.(*ast.ParenExpr); ok {
							continue // ok
						}
					}
				}

				require.Failf(t,
					"global var used",
					"Global variable in %s at %v",
					relPath, fset.Position(gen.Pos()))
			}
		}
	}
}

func TestNoSlicesArraysAndMaps(t *testing.T) {
	t.Parallel()

	filesToCheck := []string{
		"./generator.go",
	}

	for _, relPath := range filesToCheck {
		absPath, err := filepath.Abs(relPath)
		require.NoError(t, err)

		fset := token.NewFileSet()
		node, err := parser.ParseFile(fset, absPath, nil, parser.AllErrors)
		require.NoError(t, err)

		ast.Inspect(node, func(n ast.Node) bool {
			switch tt := n.(type) {
			case *ast.ArrayType:
				require.Failf(t, "array or slice used",
					"File %s uses array/slice type at position %v",
					relPath, fset.Position(tt.Pos()))

			case *ast.MapType:
				require.Failf(t, "map used",
					"File %s uses map type at position %v",
					relPath, fset.Position(tt.Pos()))
			}

			return true
		})
	}
}
