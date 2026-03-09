//go:build model_test

package cardcrypter

import (
	"crypto/aes"
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"runtime"
	"slices"
	"testing"
	"testing/synctest"
	"time"

	"github.com/stretchr/testify/require"
)

func TestEncryptDecryptWithCardIDInAAD(t *testing.T) {
	key := testKey(t)
	cards := []Card{
		{ID: "card-1", Number: CardNumber{'1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6'}},
		{ID: "card-2", Number: CardNumber{'4', '2', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'}},
	}

	crypter := New(WithWorkers(4))

	ct, err := crypter.Encrypt(cards, key)
	require.NoError(t, err)
	require.Len(t, ct, len(cards))

	ids := []string{"card-1", "card-2"}

	dec, err := decrypt(t, ct, ids, key)
	require.NoError(t, err)
	require.Len(t, dec, len(cards))

	require.Equal(t, cards[0].Number, dec[0])
	require.Equal(t, cards[1].Number, dec[1])
}

func TestDecryptWrongCardIDFails(t *testing.T) {
	key := testKey(t)

	card := Card{
		ID:     "real-card-id",
		Number: CardNumber{'1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6'},
	}

	crypter := New(WithWorkers(1))

	ct, err := crypter.Encrypt([]Card{card}, key)
	require.NoError(t, err)

	_, err = decrypt(t, ct, []string{"fake-card-id"}, key)
	require.Error(t, err)
}

func TestEncryptEmptySlice(t *testing.T) {
	c := New(WithWorkers(4))
	key := testKey(t)

	out, err := c.Encrypt(nil, key)
	require.NoError(t, err)
	require.Len(t, out, 0)

	out, err = c.Encrypt([]Card{}, key)
	require.NoError(t, err)
	require.Len(t, out, 0)
}

func TestEncryptNegativeWorkers(t *testing.T) {
	key := testKey(t)
	cards := testCards(t, 1)

	c := New(WithWorkers(0))
	_, err := c.Encrypt(cards, key)
	require.Error(t, err)

	c = New(WithWorkers(-10))
	_, err = c.Encrypt(cards, key)
	require.ErrorContains(t, err, "negative workers")
}

func TestEncryptWithInvalidKey(t *testing.T) {
	key := []byte("123")
	cards := testCards(t, 1)

	c := New()
	_, err := c.Encrypt(cards, key)

	require.ErrorContains(t, err, "invalid key")
	require.ErrorIs(t, err, aes.KeySizeError(3))
}

func TestEncryptCryptoRandReaderCall(t *testing.T) {
	reader := forceMockReader(t)

	cards := testCards(t, 10)
	tmp := slices.Clone(cards)
	key := testKey(t)

	c := New(WithWorkers(4))

	res, err := c.Encrypt(cards, key)
	require.NoError(t, err)
	require.Equal(t, 10, len(res))

	require.EqualValues(t, 10, reader.calls.Load())
	require.Equal(t, tmp, cards)
}

func TestEncryptWorkers(t *testing.T) {
	key := testKey(t)
	cards := testCards(t, 10_000)
	prev := runtime.NumGoroutine()

	workers := runtime.GOMAXPROCS(-1)
	crypter := New(WithWorkers(workers))
	gNum := inspectNumGoroutines(t, func() {
		ct, err := crypter.Encrypt(cards, key)
		require.NoError(t, err)
		require.Len(t, ct, len(cards))
	})

	require.LessOrEqual(t, gNum, workers)
	require.Equal(t, prev, runtime.NumGoroutine())
}

func TestEncryptWorkersFallback(t *testing.T) {
	key := testKey(t)
	cards := testCards(t, 10_000)

	prev := runtime.GOMAXPROCS(-1)
	t.Cleanup(func() {
		runtime.GOMAXPROCS(prev)
	})

	const workers = 123
	runtime.GOMAXPROCS(workers)

	crypter := New()
	gNum := inspectNumGoroutines(t, func() {
		ct, err := crypter.Encrypt(cards, key)
		require.NoError(t, err)
		require.Len(t, ct, len(cards))
	})

	require.InDelta(t, gNum, workers, 10)
}

func TestEncryptWorkersLimit(t *testing.T) {
	key := testKey(t)
	cards := testCards(t, 1000)

	crypter := New(WithWorkers(100000000000000))
	gNum := inspectNumGoroutines(t, func() {
		ct, err := crypter.Encrypt(cards, key)
		require.NoError(t, err)
		require.Len(t, ct, len(cards))
	})

	require.LessOrEqual(t, gNum, 1000)
}

func TestGolden(t *testing.T) {
	mockReaderWithConstant(t)

	key := testKey(t)
	cards := []Card{
		{ID: "card-1", Number: CardNumber{'1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6'}},
		{ID: "card-2", Number: CardNumber{'4', '2', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'}},
	}

	crypter := New()
	ct, err := crypter.Encrypt(cards, key)
	require.NoError(t, err)
	require.Len(t, ct, len(cards))

	slices.Sort(ct)
	require.Equal(t, "313131313131313131313131d382eb39f26d725f4616694b2a0fde33cbc718eaf7b4f2d2817e4ce16e4cacd5", ct[0])
	require.Equal(t, "313131313131313131313131d682e83df76b75574f166849290bdb3590ee92ef27190a828d801187d567faed", ct[1])
}

func TestWait(t *testing.T) {
	mockReaderWithTimeout(t, time.Hour*9999)

	synctest.Test(t, func(t *testing.T) {
		key := testKey(t)
		cards := testCards(t, 10)

		crypter := New(WithWorkers(3))

		ct, err := crypter.Encrypt(cards, key)
		require.NoError(t, err)
		require.Len(t, ct, len(cards))
	})
}

func TestNoChannels(t *testing.T) {
	filesToCheck := []string{
		"./encrypt.go",
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
					_, ok = makeExpr.Args[0].(*ast.ChanType)
					require.False(t, ok, "сhannels are prohibited in this assignment.")
				}
			}

			return true
		})
	}
}
