package queue;

import java.util.Objects;

public class LinkedQueue extends AbstractQueue{
    private Element begin;
    private Element end;


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

        Element newElement = new Element(obj, null);
        if (size == 0) {
            begin = newElement;
            end = newElement;
        } else {
            end.next = newElement;
            end = newElement;
        }
        size++;
    }

    // Pred: size > 0
    // Post: return q[1] && q' == q
    public Object element() {
        assert !isEmpty();

        return begin.value;
    }

    // Pred: size > 0
    // Post: return q[1] && q' == [q[2],..., q[size]] && size' == size - 1
    public Object dequeue() {
        assert !isEmpty();

        Element subElement = begin;
        begin = begin.next;
        size--;
        return subElement.value;
    }


    // Pred: true
    // Post: size == 0 && q' == []
    public void clear() {
        begin = null;
        end = null;
        size = 0;
    }

    private class Element {
        private final Object value;
        private Element next;

        public Element(Object value, Element next) {
            assert value != null;

            this.value = value;
            this.next = next;
        }
    }

    @Override
    protected int[] indexes(Object obj) {
        int[] result = new int[2];
        result[0] = -1;
        result[1] = -1;
        Element flag = begin;
        for (int i = 0; i < size; i++) {
            if (flag.value.equals(obj)) {
                if (result[0] == -1) {
                    result[0] = i;
                }
                result[1] = i;
            }
            flag = flag.next;
        }
        return result;
    }

}
