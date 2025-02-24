package info.kgeorgiy.ja.balakin.iterative;

import info.kgeorgiy.java.advanced.mapper.*;

import java.util.*;
import java.util.function.Function;
import java.util.stream.*;

/**
 * This is a class that implements the {@link ParallelMapper} interface for
 * thread-safety execution of operations on {@link List}.
 *
 * @author DmitruBalakin
 */
public class ParallelMapperImpl implements ParallelMapper {

    private final List<Thread> threads;
    private final TasksStack tasks;

    private final Runnable THREAD_FUNCTION = () -> {
        try {
            while (!Thread.currentThread().isInterrupted()) {
                runNext();
            }
        } catch (InterruptedException ignored) {
        } finally {
            Thread.currentThread().interrupt();
        }
    };

    private void runNext() throws InterruptedException {
        this.tasks.popTask().run();
    }

    /**
     * Constructor of {@link ParallelMapperImpl}.
     *
     * @param threads creates this count threads
     */
    public ParallelMapperImpl(int threads) {
        // :NOTE: magic constant
        tasks = new TasksStack();
        // :NOTE: Точка относится к следующему методу, так что ее принято писать с новой строки
        this.threads = IntStream.range(0, threads)
                .mapToObj(i -> new Thread(THREAD_FUNCTION))
                .peek(Thread::start)
                .collect(Collectors.toList());
    }

    /**
     * Maps the elements of a list using a given function in parallel.
     *
     * @param f    the function to apply to each element
     * @param args the list containing the elements
     * @param <T>  the type of the input elements
     * @param <R>  the type of the result elements
     * @return a list containing the results of applying the function to the elements
     * @throws InterruptedException if the operation is interrupted
     */
    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
        var answers = new ParallelList<R>(Collections.nCopies(args.size(), null));
        IntStream.range(0, args.size()).forEach(ind -> {
            try {
                tasks.addTask(() -> answers.set(ind, f.apply(args.get(ind))));
            } catch (InterruptedException ignored) {
            }
        });

        return answers.toList();
    }

    /**
     * Closes this parallel mapper and interrupts all underlying threads.
     */
    @Override
    public void close() {
        threads.forEach(Thread::interrupt);
        threads.forEach(t -> {
            while (true) {
                try {
                    t.join();
                    break;
                } catch (InterruptedException ignored) {
                }
            }
        });
    }

    private static class ParallelList<R> {
        private final List<R> list;
        private int cnt;

        ParallelList(List<R> list) {
            this.list = new ArrayList<>(list);
            cnt = 0;
        }

        public void set(int ind, R value) {
            synchronized (list) {
                list.set(ind, value);
                changeCnt();
            }
        }

        private synchronized void changeCnt() {
            cnt++;
            if (cnt == list.size()) {
                notify();
            }
        }
        synchronized void join() throws InterruptedException {
            while (cnt < list.size()) {
                wait();
            }
        }
        synchronized List<R> toList() throws InterruptedException {
            join();
            return list;
        }
    }

    private static class TasksStack {
        private final Stack<Runnable> stack;

        public TasksStack() {
            stack = new Stack<>();
        }

        public synchronized Runnable popTask() throws InterruptedException{
            while (stack.isEmpty()) {
                wait();
            }

            var res = stack.pop();
            notify();
            return res;

        }
        public synchronized void addTask(Runnable task) throws InterruptedException {
            stack.add(task);
            notify();
        }
    }
}