package info.kgeorgiy.ja.balakin.arrayset;

import java.util.*;

public class ArraySet<E extends Comparable<E>> extends AbstractSet<E> implements SortedSet<E> {

    private final List<E> data;
    private final Comparator<? super E> comparator;

    public ArraySet() {
        this( null, null);
    }

    public ArraySet(Comparator<? super E> comparator) {
        this(null, comparator);
    }

    public ArraySet(Collection<? extends E> collection) {
        this(collection, null);
    }

    public ArraySet(Collection<? extends E> collection, Comparator<? super E> comparator) {
        this.comparator = comparator;
        if (collection == null) {
            // List.of()
            this.data = new ArrayList<>();
            return;
        }

        Set<E> st = new TreeSet<>(comparator);
        st.addAll(collection);
        this.data = new ArrayList<>(st);
    }


    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    private boolean checkSubsetArgsIncorrect(E a, E b) {
        if (comparator != null) {
            return comparator.compare(a, b) > 0;
        }

        return a.compareTo(b) > 0;
    }

    private int getPosition(E el) {
        int ind = Collections.binarySearch(data, el, comparator);
        if (ind < 0) {
            ind = -(ind + 1);
        }

        return ind;
    }

    // O (n log n)
    private SortedSet<E> subSetWithPositions(int from, int to) {
        // O (n log n)
        return new ArraySet<>(data.subList(from, to), comparator);
    }

    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        if (checkSubsetArgsIncorrect(fromElement, toElement)) {
            // Message?
            throw new IllegalArgumentException("s1");
        }

        return subSetWithPositions(getPosition(fromElement), getPosition(toElement));
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return subSetWithPositions(0, getPosition(toElement));
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return subSetWithPositions(getPosition(fromElement), size());
    }

    @Override
    public E first() {
        return data.getFirst();
    }

    @Override
    public E last() {
        return data.getLast();
    }

    @Override
    public int size() {
        return data.size();
    }

    @Override
    public boolean isEmpty() {
        return data.isEmpty();
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean contains(Object o) {
        return Collections.binarySearch(data, (E) o, comparator) >= 0;
    }

    @Override
    public Iterator<E> iterator() {
        return Collections.unmodifiableList(data).iterator();
    }

    @Override
    public Object[] toArray() {
        return data.toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return data.toArray(a);
    }

    @Override
    public boolean remove(Object o) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }
}
