import org.jetbrains.annotations.NotNull;

public interface Publisher {
    @FunctionalInterface
    interface Callback {
        void onEvent(@NotNull String event);
    }

    void subscribe(Callback callback);
}
