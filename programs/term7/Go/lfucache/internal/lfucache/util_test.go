package lfucache

import "iter"

func collect[K comparable, V any](iterator iter.Seq2[K, V]) ([]K, []V) {
	keys := make([]K, 0)
	values := make([]V, 0)

	for k, v := range iterator {
		keys = append(keys, k)
		values = append(values, v)
	}

	return keys, values
}
