//go:build fips_test

package cardcrypter

import (
	"testing"

	"github.com/stretchr/testify/require"
)

// TestHandlePanicWithFips
// Use NewGCM, not NewGCMWithRandomNonce (for educational purposes)
func TestHandlePanicWithFips(t *testing.T) {
	key := testKey(t)
	cards := testCards(t, 1000)

	crypter := New()
	_, err := crypter.Encrypt(cards, key)
	require.ErrorContains(t, err, "use of GCM with arbitrary IVs is not allowed in FIPS 140-only mode")
}
