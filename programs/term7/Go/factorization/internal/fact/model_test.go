//go:build model_test

package fact

import (
	"bufio"
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"math"
	"math/rand/v2"
	"path/filepath"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"
	"unsafe"

	"github.com/stretchr/testify/require"
)

var _ Factorizer = (*factorizerImpl)(nil)

func TestNoRedundantInternalState(t *testing.T) {
	require.LessOrEqual(t, unsafe.Sizeof(factorizerImpl{}), unsafe.Sizeof(int64(0))*2)
}

func TestMixedCancel(t *testing.T) {
	runCancel(t, &testCancel{
		name:         "input",
		factWorkers:  1000,
		writeWorkers: 1000,
		numbers:      generateNumbers(1_000_000),
		doneSleep:    time.Millisecond * 100,
		beforeSleep:  0,
		err: func(t require.TestingT, err error, i ...any) {
			require.True(t, errors.Is(err, ErrWriterInteraction) || errors.Is(err, ErrFactorizationCancelled))
		},
		writer: newSleepErrorWriter(time.Millisecond*100, errors.New("123")),
	})
}

func TestWriterHasInternalErrorCancel(t *testing.T) {
	var errWriteInternalFail = errors.New("writer internally failed" + strconv.FormatInt(rand.N[int64](10e9), 10))

	runCancel(t, &testCancel{
		name:         "input",
		factWorkers:  100,
		writeWorkers: 100000,
		numbers:      generateNumbers(10_000_000),
		doneSleep:    -1,
		beforeSleep:  0,
		err: func(t require.TestingT, err error, i ...any) {
			require.ErrorIs(t, err, errWriteInternalFail)
		},
		writer: newSleepErrorWriter(time.Millisecond*100, errWriteInternalFail),
	})
}

func TestWriterErrorCancel(t *testing.T) {
	runCancel(t, &testCancel{
		name:         "input",
		factWorkers:  100,
		writeWorkers: 100000,
		numbers:      generateNumbers(10_000_000),
		doneSleep:    -1,
		beforeSleep:  0,
		err: func(t require.TestingT, err error, i ...any) {
			require.ErrorIs(t, err, ErrWriterInteraction)
		},
		writer: newSleepErrorWriter(time.Millisecond*100, errors.New("123")),
	})
}

func TestCancelInput(t *testing.T) {
	runCancel(t, &testCancel{
		name:         "input",
		factWorkers:  1,
		writeWorkers: 1,
		numbers:      []int{1},
		doneSleep:    0,
		beforeSleep:  3 * time.Second,
		err: func(t require.TestingT, err error, i ...any) {
			require.ErrorIs(t, err, ErrFactorizationCancelled)
		},
		writer: newSleepWriter(time.Second * 5),
	})
}

func TestCancelWriterNoOp(t *testing.T) {
	f, err := New()
	require.NoError(t, err)

	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	numbers := generateNumbers(1_000_000)
	writer := newWriter()

	err = f.Factorize(ctx, numbers, writer)
	require.ErrorIs(t, err, ErrFactorizationCancelled)

	require.Zero(t, len(writer.String()))
}

func TestWriterContextDone(t *testing.T) {
	f, err := New(
		WithFactorizationWorkers(100),
		WithWriteWorkers(1),
	)
	require.NoError(t, err)

	numbers := generateNumbers(1000)
	writer := newSleepWriter(time.Second)

	ctx, _ := context.WithTimeout(context.Background(), time.Second*3)

	err = f.Factorize(ctx, numbers, writer)
	require.ErrorIs(t, err, ErrFactorizationCancelled)
}

func TestCancelLow(t *testing.T) {
	runCancel(t, &testCancel{
		name:         "low",
		factWorkers:  5,
		writeWorkers: 1,
		numbers:      generateNumbers(10),
		doneSleep:    time.Second * 3,
		err: func(t require.TestingT, err error, i ...any) {
			require.ErrorIs(t, err, ErrFactorizationCancelled)
		},
		writer: newSleepWriter(time.Second * 5),
	})
}

func TestInvalidOptions(t *testing.T) {
	deferrableLeakDetection(t)

	t.Run("fact workers", func(t *testing.T) {
		_, err := New(
			WithFactorizationWorkers(-1),
			WithWriteWorkers(1),
		)

		require.ErrorContains(t, err, "factorization")
		require.ErrorContains(t, err, "-1")
	})

	t.Run("write workers", func(t *testing.T) {
		_, err := New(
			WithFactorizationWorkers(1),
			WithWriteWorkers(-1),
		)

		require.ErrorContains(t, err, "write")
		require.ErrorContains(t, err, "-1")
	})
}

func TestNoGoroutineLeak(t *testing.T) {
	deferrableLeakDetection(t)

	testErr := errors.New("test error")
	ctx, cancel := context.WithTimeoutCause(context.Background(), time.Second, testErr)

	t.Cleanup(func() {
		cancel()
	})

	fact := newFactorizer(t, 1000, 1)
	numbers := generateNumbers(1_000_000)

	err := fact.Factorize(ctx, numbers, newSleepWriter(time.Millisecond))

	// see context.Cause function
	require.ErrorIs(t, err, testErr)
	require.LessOrEqual(t, runtime.NumGoroutine(), 3)
}

func TestGoldenOutput(t *testing.T) {
	deferrableLeakDetection(t)

	testCases := []struct {
		name    string
		numbers []int
		opts    []FactorizeOption
		want    []string
	}{
		{
			name:    "positive",
			numbers: []int{1, 2, 3, 4, 5},
			opts: []FactorizeOption{
				WithFactorizationWorkers(2),
				WithWriteWorkers(2),
			},
			want: []string{
				"1 = 1",
				"2 = 2",
				"3 = 3",
				"4 = 2 * 2",
				"5 = 5",
			},
		},
		{
			name:    "negative",
			numbers: []int{0, 100, -17, 25, 38},
			opts: []FactorizeOption{
				WithFactorizationWorkers(1),
				WithWriteWorkers(1),
			},
			want: []string{
				"0 = 0",
				"100 = 2 * 2 * 5 * 5",
				"-17 = -1 * 17",
				"25 = 5 * 5",
				"38 = 2 * 19",
			},
		},
		{
			name:    "big",
			numbers: []int{1073741824, 4, 4},
			opts: []FactorizeOption{
				WithFactorizationWorkers(100),
				WithWriteWorkers(1100),
			},
			want: []string{
				"4 = 2 * 2",
				"4 = 2 * 2",
				"1073741824 = 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2",
			},
		},
		{
			name:    "simple",
			numbers: []int{10, 4, 4, 12, 15, 27, 33, 19, 14, -5, -10, -20},
			opts: []FactorizeOption{
				WithFactorizationWorkers(5),
				WithWriteWorkers(5),
			},
			want: []string{
				"10 = 2 * 5",
				"4 = 2 * 2",
				"4 = 2 * 2",
				"12 = 2 * 2 * 3",
				"15 = 3 * 5",
				"27 = 3 * 3 * 3",
				"33 = 3 * 11",
				"19 = 19",
				"14 = 2 * 7",
				"-5 = -1 * 5",
				"-10 = -1 * 2 * 5",
				"-20 = -1 * 2 * 2 * 5",
			},
		},
		{
			name:    "empty",
			numbers: []int{},
			opts: []FactorizeOption{
				WithFactorizationWorkers(1),
				WithWriteWorkers(1),
			},
			want: []string{},
		},
	}

	for _, tt := range testCases {
		t.Run(tt.name, func(t *testing.T) {
			fact, err := New(tt.opts...)
			require.NoError(t, err)

			writer := newWriter()
			err = fact.Factorize(context.Background(), tt.numbers, writer)
			require.NoError(t, err)

			facts := getFact(writer)
			slices.Sort(tt.want)
			slices.Sort(facts)

			require.Equal(t, tt.want, facts)
		})
	}
}

type TestFactorizationCorrectness struct {
	factWorkers  int
	writeWorkers int
	input        []int
}

func (tc TestFactorizationCorrectness) Run(t *testing.T) {
	deferrableLeakDetection(t)

	factorization := newFactorizer(t, tc.factWorkers, tc.writeWorkers)

	writer := newWriter()
	err := factorization.Factorize(context.Background(), tc.input, writer)
	require.NoError(t, err)

	scanner := bufio.NewScanner(strings.NewReader(writer.sb.String()))
	allNums := make([]int, 0, len(tc.input))

	for scanner.Scan() {
		line := scanner.Text()
		num, res := parseLine(t, line)
		require.True(t, checkFactorization(num, res))
		allNums = append(allNums, num)
	}

	require.NoError(t, scanner.Err())

	slices.Sort(allNums)
	slices.Sort(tc.input)

	require.Equal(t, tc.input, allNums)
}

func TestCorrectness(t *testing.T) {
	deferrableLeakDetection(t)

	TestFactorizationCorrectness{
		factWorkers:  1,
		writeWorkers: 1,
		input:        []int{math.MinInt},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  1,
		writeWorkers: 1,
		input:        []int{math.MinInt + 1},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  1,
		writeWorkers: 1,
		input:        []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, math.MaxInt32 - 13},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  1,
		writeWorkers: 5,
		input:        []int{-10, -225, -100, 250, 9, 7, 5, 3, -117, -1},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  2,
		writeWorkers: 2,
		input:        []int{12, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  3,
		writeWorkers: 1,
		input:        []int{100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  4,
		writeWorkers: 3,
		input:        []int{-3, -6, -9, -12, -15, -18, -21, -24, -27, -30},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  2,
		writeWorkers: 4,
		input:        []int{200, 225, 250, 275, 300, 325, 350, 375, 400, 425, 450},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  5,
		writeWorkers: 5,
		input:        []int{500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  3,
		writeWorkers: 6,
		input:        []int{1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  2,
		writeWorkers: 2,
		input:        []int{-50, -60, -70, -80, -90, -100, -110, -120, -130, -140},
	}.Run(t)

	TestFactorizationCorrectness{
		factWorkers:  4,
		writeWorkers: 4,
		input:        []int{144, 169, 196, 225, 256, 289, 324, 361, 400, 441, 484, 529},
	}.Run(t)

	bigPrimeN := math.MaxInt32
	if math.MaxInt == math.MaxInt64 {
		bigPrimeN = 9223372036854775783
	}

	TestFactorizationCorrectness{
		factWorkers:  1,
		writeWorkers: 1,
		input:        []int{bigPrimeN},
	}.Run(t)
}

func TestNoBufferedChannels(t *testing.T) {
	deferrableLeakDetection(t)

	filesToCheck := []string{
		"./fact.go",
	}

	for _, relPath := range filesToCheck {
		absPath, err := filepath.Abs(relPath)
		require.NoError(t, err)

		fset := token.NewFileSet()
		node, err := parser.ParseFile(fset, absPath, nil, parser.AllErrors)
		require.NoError(t, err)

		ast.Inspect(node, func(n ast.Node) bool {
			if makeExpr, ok := n.(*ast.CallExpr); ok {
				if ident, ok := makeExpr.Fun.(*ast.Ident); ok && ident.Name == "make" {
					if _, ok := makeExpr.Args[0].(*ast.ChanType); ok {
						require.Equal(t, 1, len(makeExpr.Args),
							"File %s contains a buffered channel at position %v",
							relPath, fset.Position(makeExpr.Pos()))
					}
				}
			}

			return true
		})
	}
}

func TestNoMutexUsage(t *testing.T) {
	deferrableLeakDetection(t)

	filesToCheck := []string{
		"./fact.go",
	}

	for _, relPath := range filesToCheck {
		absPath, err := filepath.Abs(relPath)
		require.NoError(t, err)

		fset := token.NewFileSet()
		node, err := parser.ParseFile(fset, absPath, nil, parser.AllErrors)
		require.NoError(t, err)

		ast.Inspect(node, func(n ast.Node) bool {
			if sel, ok := n.(*ast.SelectorExpr); ok {
				if pkgIdent, ok := sel.X.(*ast.Ident); ok &&
					pkgIdent.Name == "sync" && (sel.Sel.Name == "Mutex" || sel.Sel.Name == "RWMutex") {

					require.Failf(t, "mutex used",
						"File %s uses Mutex at position %v",
						relPath, fset.Position(sel.Pos()))
				}
			}

			return true
		})
	}
}
