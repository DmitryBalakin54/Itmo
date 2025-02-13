@FunctionalInterface
interface Callable {
    void call();
}

public class Run {
    private static void doSome(Callable callable) {
    }

    public static void main(String[] args) {
        doSome(() -> System.out.println("Hello!"));
    }
}
