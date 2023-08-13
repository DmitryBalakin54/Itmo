package queue;

public abstract class AbstractQueue implements Queue {
    protected int size;

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int indexOf(Object obj) {
        return indexes(obj)[0];
    }

    public int lastIndexOf(Object obj) {
        return indexes(obj)[1];
    }

    protected abstract int[] indexes(Object obj);
}
