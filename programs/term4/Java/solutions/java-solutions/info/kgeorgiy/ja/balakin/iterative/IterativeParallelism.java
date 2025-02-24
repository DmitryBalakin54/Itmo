package info.kgeorgiy.ja.balakin.iterative;

import info.kgeorgiy.java.advanced.iterative.NewScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Predicate;
import java.lang.Thread;
import java.util.stream.Stream;
import java.util.function.Function;

/**
 * This class implements interface {@link NewScalarIP}is able to find the maximum value, the minimum value and check the condition for all elements or
 * for any element in the {@link List}. The class has these methods for both {@link List} and {@link List} where these operations are applied
 * to each step element starting from 0.
 *
 * @author DmitryBalakin
 */
public class IterativeParallelism implements NewScalarIP {


    private final ParallelMapper mapper;

    /**
     * Constructs an IterativeParallelism object with the specified ParallelMapper.
     *
     * @param mapper the ParallelMapper to use for parallel execution
     */
    public IterativeParallelism(final ParallelMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * Constructs an IterativeParallelism object with no ParallelMapper.
     */
    public IterativeParallelism() {
        mapper = null;
    }


    private <T, E> E apply(int amount,
                           List<? extends T> values,
                           Function<Stream<? extends T>, E> threadFunction,
                           Function<Stream<? extends E>, E> resultFunction, int step) throws InterruptedException {

        int len = values.size();
        amount = Math.min(Math.max(amount, 1), len);

        int offset = len / amount;
        int amountOfLongSegments = len % amount;

        List<Thread> threads = new ArrayList<>();
        List<E> results = new ArrayList<>(Collections.nCopies(amount, null));
        List<Stream<? extends T>> newValues = new ArrayList<>();

        int bound = 0;
        for (int i = 0; i < amount; i++) {
            int ind = i;
            int begin = bound;
            int end = begin + offset + (i < amountOfLongSegments ? 1 : 0);
            bound = end;

            var data = subListStreamWithStep(values, begin, end, step).filter(Objects::nonNull);

            if (mapper == null) {
                var thread = new Thread(() -> results.set(
                        ind, threadFunction.apply(data))
                );
                threads.add(thread);
                thread.start();
            } else {
                newValues.add(data);
            }

        }

        if (mapper == null) {
            // :NOTE: Если один из тредов сломается, другие не будут заджоинены
            threads.forEach(t -> {
                while (true) {
                    try {
                        t.join();
                        break;
                    } catch (InterruptedException ignored) {
                    }
                }
            });

            return answer(results, resultFunction);
        } else {
            return answer(mapper.map(threadFunction, newValues), resultFunction);
        }
    }

    private <E> E answer(List<? extends E> data, Function<Stream<? extends E>, E> func) {
        return func.apply(data.stream().filter(Objects::nonNull));
    }


    private <T> Stream<T> subListStreamWithStep(List<T> lst, int from, int to, int step) {
        if (step == 1) {
            return lst.subList(from, to).stream();
        }

        from += from % step == 0 ? 0 : step - from % step;
        if (from < to) {
            var subLst = lst.subList(from, to);
            return Stream.iterate(0, i -> i + step).
                    limit((to - from - 1) / step + 1).
                    map(subLst::get);
        }

        // :NOTE: Stream.of
        return lst.subList(0, 0).stream();
    }

    /**
     * Finds the maximum element in the input list using the specified number of threads and comparator.
     *
     * @param threads the number of threads to use for computation.
     * @param values the list of values to find the maximum element from.
     * @param comparator the comparator to determine the order of elements.
     * @param step the size of the step for processing the elements of the list.
     * @param <T> the type of elements in the list.
     * @return the maximum element in the list, or null if the list is empty.
     * @throws InterruptedException if any thread is interrupted during computation.
     */
    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator, int step) throws InterruptedException {
        return apply(threads,
                values,
                s -> s.max(comparator).orElse(null),
                s -> s.max(comparator).orElse(null), step);
    }

    /**
     * Finds the minimum element in the input list using the specified number of threads and comparator.
     *
     * @param threads the number of threads to use for computation.
     * @param values the list of values to find the minimum element from.
     * @param comparator the comparator to determine the order of elements.
     * @param step the size of the step for processing the elements of the list.
     * @param <T> the type of elements in the list.
     * @return the minimum element in the list, or null if the list is empty.
     * @throws InterruptedException if any thread is interrupted during computation.
     */
    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator, int step) throws InterruptedException {
        return maximum(threads, values, comparator.reversed(), step);
    }

    /**
     * Checks if all elements in the input list satisfy the given predicate using the specified number of threads.
     *
     * @param threads the number of threads to use for computation.
     * @param values the list of values to check against the predicate.
     * @param predicate the predicate to apply to the elements.
     * @param step the size of the step for processing the elements of the list.
     * @param <T> the type of elements in the list.
     * @return true if all elements satisfy the predicate, false otherwise.
     * @throws InterruptedException if any thread is interrupted during computation.
     */
    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return apply(threads,
                values,
                s -> s.allMatch(predicate),
                s -> s.allMatch(v -> v), step);
    }

    /**
     * Checks if any element in the input list satisfies the given predicate using the specified number of threads.
     *
     * @param threads the number of threads to use for computation.
     * @param values the list of values to check against the predicate.
     * @param predicate the predicate to apply to the elements.
     * @param step the size of the step for processing the elements of the list.
     * @param <T> the type of elements in the list.
     * @return true if any element satisfies the predicate, false otherwise.
     * @throws InterruptedException if any thread is interrupted during computation.
     */
    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return !all(threads, values, predicate.negate(), step);
    }

    /**
     * Counts the number of elements in the input list that satisfy the given predicate using the specified number of threads.
     *
     * @param threads the number of threads to use for computation.
     * @param values the list of values to count against the predicate.
     * @param predicate the predicate to apply to the elements.
     * @param step the size of the step for processing the elements of the list.
     * @param <T> the type of elements in the list.
     * @return the number of elements satisfying the predicate.
     * @throws InterruptedException if any thread is interrupted during computation.
     */
    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return apply(threads,
                values,
                s -> (int) s.filter(predicate).count(),
                s -> s.mapToInt(Integer::intValue).sum(), step);
    }
}
