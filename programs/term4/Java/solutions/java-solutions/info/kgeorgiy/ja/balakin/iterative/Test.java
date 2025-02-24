package info.kgeorgiy.ja.balakin.iterative;
import java.util.*;
public class Test {
    public static void main(String[] args) throws InterruptedException {
        var it = new IterativeParallelism(new ParallelMapperImpl(10));
        System.out.println(List.of(0, 1, 2, 3, -12, 5, 14, 55));
        System.out.println(it.maximum(3, List.of(0, 1, 2, 3, -12, 5, 14, 55, 33), Integer::compareTo, 1));
    }
}
