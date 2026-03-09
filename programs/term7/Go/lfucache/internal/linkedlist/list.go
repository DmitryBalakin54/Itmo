package linkedlist

import (
	"iter"
)

type Element[T any] struct {
	prev  *Element[T]
	next  *Element[T]
	Value T
}

type LinkedList[T any] struct {
	head *Element[T]
	tail *Element[T]
	size int
}

func New[T any]() *LinkedList[T] {
	list := &LinkedList[T]{
		head: &Element[T]{},
		tail: &Element[T]{},
		size: 0,
	}

	list.head.next = list.tail
	list.tail.prev = list.head

	return list
}

func (list *LinkedList[T]) Size() int {
	return list.size
}

func (list *LinkedList[T]) IsEmpty() bool {
	return list.size == 0
}

func (list *LinkedList[T]) Front() *Element[T] {
	if list.size == 0 {
		return nil
	}

	return list.head.next
}

func (list *LinkedList[T]) Back() *Element[T] {
	if list.size == 0 {
		return nil
	}

	return list.tail.prev
}

func (list *LinkedList[T]) PushFront(value T) {
	list.InsertAfter(value, list.head)
}

func (list *LinkedList[T]) PushBack(value T) {
	list.InsertBefore(value, list.tail)
}

func (list *LinkedList[T]) PushElementFront(element *Element[T]) {
	list.InsertElementAfter(element, list.head)
}

func (list *LinkedList[T]) PushElementBack(element *Element[T]) {
	list.InsertElementBefore(element, list.tail)
}

func (list *LinkedList[T]) MoveToFront(element *Element[T]) {
	list.Remove(element)
	list.InsertElementAfter(element, list.head)
}

func (list *LinkedList[T]) MoveToBack(element *Element[T]) {
	list.Remove(element)
	list.InsertElementBefore(element, list.tail)
}

func (list *LinkedList[T]) InsertAfter(value T, at *Element[T]) {
	element := &Element[T]{
		at,
		at.next,
		value,
	}

	list.InsertElementAfter(element, at)
}

func (list *LinkedList[T]) InsertElementAfter(element *Element[T], at *Element[T]) {
	element.prev = at
	element.next = at.next
	at.next.prev = element
	at.next = element
	list.size++
}

func (list *LinkedList[T]) InsertBefore(value T, at *Element[T]) {
	element := &Element[T]{
		at.prev,
		at,
		value,
	}

	list.InsertElementBefore(element, at)
}

func (list *LinkedList[T]) InsertElementBefore(element *Element[T], at *Element[T]) {
	element.next = at
	element.prev = at.prev
	at.prev.next = element
	at.prev = element
	list.size++
}

func (list *LinkedList[T]) Remove(element *Element[T]) {
	element.prev.next = element.next
	element.next.prev = element.prev
	element.prev = nil
	element.next = nil
	list.size--
}

func (list *LinkedList[T]) GetNext(element *Element[T]) *Element[T] {
	if element.next != list.tail {
		return element.next
	}

	return nil
}

func (list *LinkedList[T]) GetPrev(element *Element[T]) *Element[T] {
	if element.prev != list.head {
		return element.prev
	}

	return nil
}

func (list *LinkedList[T]) All() iter.Seq[T] {
	return func(yield func(T) bool) {
		for cur := list.head.next; cur != list.tail; cur = cur.next {
			if !yield(cur.Value) {
				return
			}
		}
	}
}

func (list *LinkedList[T]) AllReversed() iter.Seq[T] {
	return func(yield func(T) bool) {
		for cur := list.tail.prev; cur != list.head; cur = cur.prev {
			if !yield(cur.Value) {
				return
			}
		}
	}
}

func (el *Element[T]) GetPrev() *Element[T] {
	if el.prev.prev == nil {
		return nil
	}

	return el.prev
}

func (el *Element[T]) GetNext() *Element[T] {
	if el.next.next == nil {
		return nil
	}

	return el.next
}
