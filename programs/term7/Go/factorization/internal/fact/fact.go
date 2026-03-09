package fact

import (
	"context"
	"errors"
	"fmt"
	"io"
	"math"
	"runtime"
	"strconv"
	"strings"
)

var (
	ErrFactorizationCancelled = errors.New("cancelled")
	ErrWriterInteraction      = errors.New("writer interaction")
)

type Factorizer interface {
	Factorize(ctx context.Context, numbers []int, writer io.Writer) error
}

type factorizerImpl struct {
	factWorkers  int
	writeWorkers int
}

func New(opts ...FactorizeOption) (*factorizerImpl, error) {
	impl := &factorizerImpl{
		factWorkers:  runtime.GOMAXPROCS(0),
		writeWorkers: runtime.GOMAXPROCS(0),
	}

	for _, opt := range opts {
		opt(impl)
	}

	if impl.factWorkers < 1 {
		return nil, fmt.Errorf("factorization: factWorkers must be at least 1, got %d", impl.factWorkers)
	}

	if impl.writeWorkers < 1 {
		return nil, fmt.Errorf("factorization: writeWorkers must be at least 1, got %d", impl.writeWorkers)
	}

	return impl, nil
}

type FactorizeOption func(*factorizerImpl)

func WithFactorizationWorkers(workers int) FactorizeOption {
	return func(f *factorizerImpl) {
		f.factWorkers = workers
	}
}

func WithWriteWorkers(workers int) FactorizeOption {
	return func(f *factorizerImpl) {
		f.writeWorkers = workers
	}
}

func (f *factorizerImpl) Factorize(
	ctx context.Context,
	numbers []int,
	writer io.Writer,
) error {
	if ctx.Err() != nil {
		return fmt.Errorf("%w: %w", ErrFactorizationCancelled, ctx.Err())
	}

	in := make(chan int)
	out := make(chan string)
	errCh := make(chan error)
	doneWrite := make(chan struct{})
	doneFact := make(chan struct{})

	var errRes error

	defer close(doneFact)
	defer close(doneWrite)
	defer close(errCh)

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	go func() {
		defer close(in)

		for _, n := range numbers {
			select {
			case <-ctx.Done():
				return
			case in <- n:
			}
		}
	}()

	for i := 0; i < f.factWorkers; i++ {
		go func() {
		outer:
			for {
				select {
				case <-ctx.Done():
					break outer
				case n, ok := <-in:
					if !ok {
						break outer
					}

					select {
					case <-ctx.Done():
						break outer
					case out <- format(n, factorize(n)):
					}
				}
			}

			doneFact <- struct{}{}
		}()
	}

	for i := 0; i < f.writeWorkers; i++ {
		go func() {
		outer:
			for {
				select {
				case <-ctx.Done():
					break outer
				case s, ok := <-out:
					if !ok {
						break outer
					}

					select {
					case <-ctx.Done():
						break outer
					default:
						if _, err := writer.Write([]byte(s + "\n")); err != nil {
							select {
							case <-ctx.Done():
							case errCh <- fmt.Errorf("%w %w", ErrWriterInteraction, err):
								cancel()
							}

							break outer
						}
					}
				}
			}

			doneWrite <- struct{}{}
		}()
	}

	finished := 0
	finished1 := 0

outer:
	for {
		select {
		case <-doneWrite:
			finished++
			if finished == f.writeWorkers && finished1 == f.factWorkers {
				break outer
			}
		case <-doneFact:
			finished1++
			if finished1 == f.factWorkers {
				close(out)
			}
			if finished == f.writeWorkers && finished1 == f.factWorkers {
				break outer
			}
		case err := <-errCh:
			errRes = err
		}
	}

	select {
	case <-ctx.Done():
		if errRes != nil {
			return errRes
		}

		if err := context.Cause(ctx); err != nil {
			return fmt.Errorf("%w %w", ErrFactorizationCancelled, err)
		}

		return ErrFactorizationCancelled
	default:
		return nil
	}
}

func factorize(n int) []int {
	if n == 0 {
		return []int{0}
	}
	if n == 1 || n == -1 {
		return []int{n}
	}

	var res []int

	if n < 0 {
		res = append(res, -1)

		if n == math.MinInt {
			res = append(res, 2)
			n /= 2
		}

		n = -n
	}

	for i := 2; i <= n/i; i++ {
		for n%i == 0 {
			res = append(res, i)
			n /= i
		}
	}

	if n > 1 {
		res = append(res, n)
	}

	return res
}

func format(n int, factors []int) string {
	s := make([]string, len(factors))
	for i, v := range factors {
		s[i] = strconv.Itoa(v)
	}
	return fmt.Sprintf("%d = %s", n, strings.Join(s, " * "))
}
