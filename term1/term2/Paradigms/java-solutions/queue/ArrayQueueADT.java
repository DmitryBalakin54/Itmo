package queue;

import java.util.Objects;

// Model: q[1]..q[n]
// Inv: size >= 0 && forall i=1..size: q[i] != null
// q1 == q2 <=> q1.size == q2.size && forall i from (1,...,q1.size) : q1[i] = q2[i]
public class ArrayQueueADT {

    private final int START_SIZE = 2;
    private int begin;
    private int size;
    private Object[] elements;

    public ArrayQueueADT(){
        elements = new Object[START_SIZE];
    }

//    enqueue – добавить элемент в очередь;
//    element – первый элемент в очереди;
//    dequeue – удалить и вернуть первый элемент в очереди;
//    size – текущий размер очереди;
//    isEmpty – является ли очередь пустой;
//    clear – удалить все элементы из очереди.


    // Pred: true
    // Post: return q (q.size == 0 && q == [])
    public static ArrayQueueADT create() {
        ArrayQueueADT queue = new ArrayQueueADT();
        queue.elements = new Object[queue.START_SIZE];
        return queue;
    }

    // Pred: obj != null
    // Post: size' == size + 1 && q' == [q[1],...,q[size], obj]
    public static void enqueue(ArrayQueueADT queue, Object obj) {
        Objects.requireNonNull(obj);

        resize(queue);
        queue.elements[(queue.begin + queue.size) % queue.elements.length] = obj;
        queue.size++;
    }

    // Pred: true
    // Post: q' == q
    private static void resize(ArrayQueueADT queue) {
        if (queue.size == queue.elements.length) {
            Object[] newElements = new Object[queue.elements.length * 2];
            for (int i = 0; i < queue.size; i++) {
                newElements[i] = queue.elements[(queue.begin + i) % queue.elements.length];
            }
            queue.elements = newElements;
            queue.begin = 0;
        }
    }

    // Pred: size > 0
    // Post: return q[1] && q' == q
    public static Object element(ArrayQueueADT queue) {
        assert !isEmpty(queue);

        return queue.elements[queue.begin];
    }

    // Pred: size > 0
    // Post: return q[1] && q' == [q[2],..., q[size]] && size' == size - 1
    public static Object dequeue(ArrayQueueADT queue) {
        assert !isEmpty(queue);

        int subBegin = queue.begin;
        queue.begin = (queue.begin + 1) % queue.elements.length;
        queue.size--;
        return queue.elements[subBegin];
    }

    // Pred: true
    // Post: return size && q' == q
    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    // Pred: true
    // Post: return (size == 0) && q' == q
    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    // Pred: true
    // Post: size == 0 && q' == []
    public static void clear(ArrayQueueADT queue) {
        queue.begin = 0;
        queue.size = 0;
        queue.elements = new Object[queue.START_SIZE];
    }

    // Pred: true
    // Post: q' == q
    public static Object[] toArray(ArrayQueueADT queue) {
        Object[] array = new Object[queue.size];
        for (int i = 0; i < queue.size; i++) {
            array[i] = queue.elements[(queue.begin + i) % queue.elements.length];
        }
        return array;
    }
}
