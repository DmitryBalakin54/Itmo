package queue;

// Model: q[1]..q[n]
// Inv: size >= 0 && forall i=1..size: q[i] != null
// q1 == q2 <=> q1.size == q2.size && forall i from (1,...,q1.size) : q1[i] = q2[i]
public interface Queue {
    // Pred: obj != null
    // Post: size' == size + 1 && q' == [q[1],...,q[size], obj]
    void enqueue(Object obj);

    // Pred: size > 0
    // Post: return q[1] && q' == q
    Object element();

    // Pred: size > 0
    // Post: return q[1] && q' == [q[2],..., q[size]] && size' == size - 1
    Object dequeue();

    // Pred: true
    // Post: return size && q' == q
    int size();

    // Pred: true
    // Post: return (size == 0) && q' == q
    boolean isEmpty();

    // Pred: true
    // Post: size == 0 && q' == []
    void clear();

    int indexOf(Object obj);


    int lastIndexOf(Object obj);
}
