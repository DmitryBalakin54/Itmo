package fact

import (
	"context"
	"io"
	"math"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func deferrableLeakDetection(t *testing.T) {
	t.Helper()

	t.Cleanup(func() {
		gNum := runtime.NumGoroutine()

		for range 100 {
			gNum = max(gNum, runtime.NumGoroutine())
		}

		require.LessOrEqual(t, gNum, 3, "found goroutine leak in test")
	})
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

type testCancel struct {
	name         string
	factWorkers  int
	writeWorkers int
	numbers      []int
	doneSleep    time.Duration
	beforeSleep  time.Duration
	writer       io.Writer
	err          require.ErrorAssertionFunc
}

func ctxWithCancel(t *testing.T) (context.Context, context.CancelFunc) {
	t.Helper()

	return context.WithCancel(context.Background())
}

func newFactorizer(
	t *testing.T,
	factWorkers int,
	writeWorkers int,
) *factorizerImpl {
	t.Helper()

	f, err := New(
		WithFactorizationWorkers(factWorkers),
		WithWriteWorkers(writeWorkers),
	)

	require.NoError(t, err)

	return f
}

func runCancel(t *testing.T, tt *testCancel) {
	deferrableLeakDetection(t)

	ctx, cancel := ctxWithCancel(t)
	fact := newFactorizer(t, tt.factWorkers, tt.writeWorkers)

	wg := new(sync.WaitGroup)
	if tt.doneSleep != -1 {
		wg.Go(func() {
			time.Sleep(tt.doneSleep)
			cancel()
		})
	}

	time.Sleep(tt.beforeSleep)
	err := fact.Factorize(ctx, tt.numbers, tt.writer)
	wg.Wait()

	tt.err(t, err)
}

func generateNumbers(n int) []int {
	s := make([]int, 0, n)

	for i := 0; i < n; i++ {
		s = append(s, i)
	}

	return s
}

func getFact(writer TestWriter) []string {
	r := strings.TrimRight(writer.String(), "\n")

	if r == "" {
		return []string{}
	}

	return strings.Split(r, "\n")
}

type TestWriter interface {
	io.Writer
	String() string
}

type concurrentWriter struct {
	sb *strings.Builder
	mx *sync.RWMutex
}

type sleepErrorWriter struct {
	sb        *strings.Builder
	mx        *sync.RWMutex
	sleepTime time.Duration
	err       error
}

func newSleepErrorWriter(sleepTime time.Duration, err error) *sleepErrorWriter {
	return &sleepErrorWriter{
		sb:        new(strings.Builder),
		mx:        new(sync.RWMutex),
		sleepTime: sleepTime,
		err:       err,
	}
}

func (s *sleepErrorWriter) Write(_ []byte) (n int, err error) {
	time.Sleep(s.sleepTime)

	s.mx.Lock()
	defer s.mx.Unlock()

	return 0, s.err
}

type sleepWriter struct {
	sb        *strings.Builder
	mx        *sync.RWMutex
	sleepTime time.Duration
}

func newSleepWriter(sleepTime time.Duration) *sleepWriter {
	return &sleepWriter{
		sb:        new(strings.Builder),
		mx:        new(sync.RWMutex),
		sleepTime: sleepTime,
	}
}

func (s *sleepWriter) Write(p []byte) (n int, err error) {
	time.Sleep(s.sleepTime)

	s.mx.Lock()
	defer s.mx.Unlock()

	return s.sb.Write(p)
}

func newWriter() *concurrentWriter {
	return &concurrentWriter{
		sb: new(strings.Builder),
		mx: new(sync.RWMutex),
	}
}

func (c *concurrentWriter) Write(p []byte) (n int, err error) {
	c.mx.Lock()
	defer c.mx.Unlock()

	return c.sb.Write(p)
}

func (c *concurrentWriter) String() string {
	c.mx.RLock()
	defer c.mx.RUnlock()

	return c.sb.String()
}

func strToInt(t *testing.T, str string) int {
	t.Helper()

	d, err := strconv.Atoi(strings.TrimSpace(str))
	require.NoError(t, err)

	return d
}

func delimiterStringsToSliceInt(t *testing.T, delimiterStrings []string) []int {
	t.Helper()

	delimiters := make([]int, 0, len(delimiterStrings))
	for _, dString := range delimiterStrings {
		num := strToInt(t, dString)
		delimiters = append(delimiters, num)
	}

	return delimiters
}

func parseLine(t *testing.T, line string) (int, []int) {
	t.Helper()

	s := strings.Split(line, "=")
	left := strToInt(t, s[0])
	right := delimiterStringsToSliceInt(t, strings.Split(strings.ReplaceAll(strings.Join(s[1:], ""),
		" ", ""), "*"))

	return left, right
}

func checkFactorization(num int, delimiters []int) bool {
	if !slices.IsSortedFunc(delimiters, func(i, j int) int {
		return i - j
	}) {
		return false
	}

	got := 1
	for _, d := range delimiters {
		tmp := d

		if d < 0 {
			d *= -1
		}

		if !pChecker.IsPrime(d) {
			return false
		}

		got *= tmp
	}

	return num == got
}

var pChecker = newPrimeChecker()

type primeChecker struct {
	memo map[int]bool
}

func newPrimeChecker() *primeChecker {
	return &primeChecker{
		memo: make(map[int]bool),
	}
}

func (pc *primeChecker) IsPrime(n int) bool {
	if n == 1 || n == 2 {
		return true
	}

	if result, exists := pc.memo[n]; exists {
		return result
	}

	sqrtN := int(math.Sqrt(float64(n)))
	isPrime := true
	for i := 2; i <= sqrtN; i++ {
		if n%i == 0 {
			isPrime = false
			break
		}
	}

	pc.memo[n] = isPrime

	return isPrime
}
