package queue;

import java.util.Objects;

// Model: q[1]..q[n]
// Inv: size >= 0 && forall i=1..size: q[i] != null
// q1 == q2 <=> q1.size == q2.size && forall i from (1,...,q1.size) : q1[i] = q2[i]
public class ArrayQueueModule {

    private static final int START_SIZE = 2;
    private static int begin;
    private static int size;
    private static Object[] elements = new Object[START_SIZE];

//    enqueue – добавить элемент в очередь;
//    element – первый элемент в очереди;
//    dequeue – удалить и вернуть первый элемент в очереди;
//    size – текущий размер очереди;
//    isEmpty – является ли очередь пустой;
//    clear – удалить все элементы из очереди.


    // Pred: obj != null
    // Post: size' == size + 1 && q' == [q[1],...,q[size], obj]
    public static void enqueue(Object obj) {
        Objects.requireNonNull(obj);

        resize();
        elements[(begin + size) % elements.length] = obj;
        size++;
    }

    // Pred: true
    // Post: q' == q
    private static void resize() {
        if (size == elements.length) {
            Object[] newElements = new Object[elements.length * 2];
            for (int i = 0; i < size; i++) {
                newElements[i] = elements[(begin + i) % elements.length];
            }
            elements = newElements;
            begin = 0;
        }
    }

    // Pred: size > 0
    // Post: return q[1] && q' == q
    public static Object element() {
        assert !isEmpty();

        return elements[begin];
    }

    // Pred: size > 0
    // Post: return q[1] && q' == [q[2],..., q[size]] && size' == size - 1
    public static Object dequeue() {
        assert !isEmpty();

        int subBegin = begin;
        begin = (begin + 1) % elements.length;
        size--;
        return elements[subBegin];
    }

    // Pred: true
    // Post: return size && q' == q
    public static int size() {
        return size;
    }

    // Pred: true
    // Post: return (size == 0) && q' == q
    public static boolean isEmpty() {
        return size == 0;
    }


    // Pred: true
    // Post: size == 0 && q' == []
    public static void clear() {
        begin = 0;
        size = 0;
        elements = new Object[START_SIZE];
    }

    // Pred: true
    // Post: q' == q
    public static Object[] toArray() {
        Object[] array = new Object[size];
        for (int i = 0; i < size; i++) {
            array[i] = elements[(begin + i) % elements.length];
        } // EnsureCapacity?
        return array;
    }
}
