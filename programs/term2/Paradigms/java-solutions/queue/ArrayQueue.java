package queue;

import java.util.Objects;

// Model: q[1]..q[n]
// Inv: size >= 0 && forall i=1..size: q[i] != null
// q1 == q2 <=> q1.size == q2.size && forall i from (1,...,q1.size) : q1[i] = q2[i]
public class ArrayQueue extends AbstractQueue{
    private final int START_SIZE = 2;
    private int begin;
    private Object[] elements = new Object[START_SIZE];

//    enqueue – добавить элемент в очередь;
//    element – первый элемент в очереди;
//    dequeue – удалить и вернуть первый элемент в очереди;
//    size – текущий размер очереди;
//    isEmpty – является ли очередь пустой;
//    clear – удалить все элементы из очереди.


    // Pred: obj != null
    // Post: size' == size + 1 && q' == [q[1],...,q[size], obj]
    public void enqueue(Object obj) {
        Objects.requireNonNull(obj);

        resize();
        elements[(begin + size) % elements.length] = obj;
        size++;
    }

    // Pred: true
    // Post: q' == q
    private void resize() {
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
    public Object element() {
        assert !isEmpty();

        return elements[begin];
    }

    // Pred: size > 0
    // Post: return q[1] && q' == [q[2],..., q[size]] && size' == size - 1
    public Object dequeue() {
        assert !isEmpty();

        int subBegin = begin;
        begin = (begin + 1) % elements.length;
        size--;
        return elements[subBegin];
    }

    // Pred: true
    // Post: size == 0 && q' == []
    public void clear() {
        begin = 0;
        size = 0;
        elements = new Object[START_SIZE];
    }

    // Pred: true
    // Post: q' == q
    public  Object[] toArray() {
        Object[] array = new Object[size];
        for (int i = 0; i < size; i++) {
            array[i] = elements[(begin + i) % elements.length];
        }
        return array;
    }

    @Override
    protected int[] indexes(Object obj) {
        int[] result = new int[2];
        result[0] = -1;
        result[1] = -1;
        for (int i = 0; i < size; i++) {
            if (elements[(begin + i) % elements.length].equals(obj)) {
                if (result[0] == -1) {
                    result[0] = i;
                }
                result[1] = i;
            }
        }
        return result;
    }


}
