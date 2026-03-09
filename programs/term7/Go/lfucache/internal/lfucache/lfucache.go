package lfucache

import (
	"errors"
	"iter"

	"github.com/igoroutine-courses/gonature.lfucache/internal/linkedlist"
)

var ErrKeyNotFound = errors.New("key not found")

const DefaultCapacity = 5

type Cache[K comparable, V any] interface {
	Get(key K) (V, error)
	Put(key K, value V)
	All() iter.Seq2[K, V]
	Size() int
	Capacity() int
	GetKeyFrequency(key K) (int, error)
}

type dataEl[K comparable, V any] struct {
	key   K
	value V
}

type cacheElement[K comparable, V any] struct {
	listElement *linkedlist.LinkedList[dataEl[K, V]]
	frequency   int
}

func (el *cacheElement[K, V]) incrementFreq() {
	el.frequency++
}

type cacheImpl[K comparable, V any] struct {
	elements          *linkedlist.LinkedList[*cacheElement[K, V]]
	keyToElement      map[K]*linkedlist.Element[dataEl[K, V]]
	keyToCacheElement map[K]*linkedlist.Element[*cacheElement[K, V]]
	elementsPool      *linkedlist.LinkedList[dataEl[K, V]]
	capacity          int
	size              int

	view    []dataEl[K, V]
	changed bool
}

func New[K comparable, V any](capacity ...int) *cacheImpl[K, V] {
	cache := &cacheImpl[K, V]{
		capacity: DefaultCapacity,
		size:     0,
	}

	if len(capacity) != 0 {
		cache.capacity = capacity[0]
		if cache.capacity < 0 {
			panic("Capacity must be not negative number")
		}
	}

	cache.elements = linkedlist.New[*cacheElement[K, V]]()
	cache.keyToElement = make(map[K]*linkedlist.Element[dataEl[K, V]])
	cache.keyToCacheElement = make(map[K]*linkedlist.Element[*cacheElement[K, V]])
	cache.elementsPool = linkedlist.New[dataEl[K, V]]()

	firstElement := &cacheElement[K, V]{
		linkedlist.New[dataEl[K, V]](),
		1,
	}

	cache.elements.PushFront(firstElement)

	cache.view = make([]dataEl[K, V], cache.capacity)
	cache.changed = true

	return cache
}

func (l *cacheImpl[K, V]) Get(key K) (V, error) {
	if v, ok := l.keyToElement[key]; ok {
		l.changed = true

		if l.canIncrementFreq(key) {
			l.keyToCacheElement[key].Value.incrementFreq()

			return l.keyToElement[key].Value.value, nil
		}

		l.addCacheElement(v)

		return v.Value.value, nil
	}

	return *new(V), ErrKeyNotFound
}

func (l *cacheImpl[K, V]) Put(key K, value V) {
	l.changed = true

	if _, ok := l.keyToElement[key]; ok {
		l.changeValue(key, value)
		return
	}

	if l.size == l.capacity {
		l.deleteOne()
	}

	l.insert(key, value)
}

func (l *cacheImpl[K, V]) All() iter.Seq2[K, V] {
	return func(yield func(K, V) bool) {
		if l.changed {
			j := 0
			for el := range l.elements.AllReversed() {
				for nod := range el.listElement.AllReversed() {
					l.view[j] = nod
					j++
				}
			}

			l.changed = false
		}

		for i := 0; i < l.size; i++ {
			if !yield(l.view[i].key, l.view[i].value) {
				return
			}
		}
	}
}

func (l *cacheImpl[K, V]) Size() int {
	return l.size
}

func (l *cacheImpl[K, V]) Capacity() int {
	return l.capacity
}

func (l *cacheImpl[K, V]) GetKeyFrequency(key K) (int, error) {
	if v, ok := l.keyToCacheElement[key]; ok {
		return v.Value.frequency, nil
	}

	return 0, ErrKeyNotFound
}

func (l *cacheImpl[K, V]) deleteOne() {
	var del *linkedlist.Element[dataEl[K, V]]
	if l.getFirstList().IsEmpty() {
		del = l.getSecondList().Front()
		l.getSecondList().Remove(del)

		if l.getSecondList().IsEmpty() {
			l.elements.Remove(l.getSecondElement())
		}
	} else {
		del = l.getFirstList().Front()
		l.getFirstList().Remove(del)
	}

	l.elementsPool.PushElementBack(del)
	delete(l.keyToElement, del.Value.key)
	delete(l.keyToCacheElement, del.Value.key)
	l.size--
}

func (l *cacheImpl[K, V]) insert(key K, value V) {
	if l.elementsPool.IsEmpty() {
		l.getFirstList().PushBack(dataEl[K, V]{key, value})
	} else {
		el := l.elementsPool.Front()
		l.elementsPool.Remove(el)
		el.Value.key = key
		el.Value.value = value

		l.getFirstList().PushElementBack(el)
	}

	l.keyToElement[key] = l.getFirstList().Back()
	l.keyToCacheElement[key] = l.elements.Front()
	l.size++
}

func (l *cacheImpl[K, V]) changeValue(key K, value V) {
	_, _ = l.Get(key)
	l.keyToElement[key].Value.value = value
}

func (l *cacheImpl[K, V]) addCacheElement(v *linkedlist.Element[dataEl[K, V]]) {
	key := v.Value.key
	freq := l.keyToCacheElement[key].Value.frequency
	next := l.keyToCacheElement[key].GetNext()

	l.keyToCacheElement[key].Value.listElement.Remove(v)

	if next == nil || next.Value.frequency > freq+1 {
		l.elements.InsertAfter(
			l.createNewCacheElementByFreq(freq+1),
			l.keyToCacheElement[key],
		)

		l.keyToCacheElement[key].GetNext().Value.listElement.PushElementBack(v)
	} else {
		next.Value.listElement.PushElementBack(v)
	}

	l.keyToCacheElement[key] = l.keyToCacheElement[key].GetNext()
	l.keyToElement[key] = l.keyToCacheElement[key].Value.listElement.Back()
}

func (l *cacheImpl[K, V]) getFirsElement() *linkedlist.Element[*cacheElement[K, V]] {
	return l.elements.Front()
}

func (l *cacheImpl[K, V]) getFirstList() *linkedlist.LinkedList[dataEl[K, V]] {
	return l.getFirsElement().Value.listElement
}

func (l *cacheImpl[K, V]) getSecondElement() *linkedlist.Element[*cacheElement[K, V]] {
	return l.elements.Front().GetNext()
}

func (l *cacheImpl[K, V]) getSecondList() *linkedlist.LinkedList[dataEl[K, V]] {
	return l.getSecondElement().Value.listElement
}

func (l *cacheImpl[K, V]) createNewCacheElementByFreq(freq int) *cacheElement[K, V] {
	return &cacheElement[K, V]{
		linkedlist.New[dataEl[K, V]](),
		freq,
	}
}

func (l *cacheImpl[K, V]) canIncrementFreq(key K) bool {
	return l.keyToCacheElement[key].Value.listElement.Size() == 1 &&
		l.keyToCacheElement[key].GetNext() == nil &&
		l.keyToCacheElement[key].Value.frequency > 1
}
