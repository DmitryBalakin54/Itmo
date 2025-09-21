package solution;

/**
 * Distributed Dijkstra algorithm implementation.
 * All functions are called from the single main thread.
 *
 * @author <First-Name> <Last-Name> // todo: replace with your name
 */
public class DijkstraProcessJavaImpl implements DijkstraProcess {
    private final Environment env;

    public DijkstraProcessJavaImpl(Environment env) {
        this.env = env;
    }

    @Override
    public void onComputationStart() {
        throw new UnsupportedOperationException("Not implemented");
    }

    @Override
    public void onMessage(int srcId, Object message) {
        throw new UnsupportedOperationException("Not implemented");
    }

    @Override
    public Long getDistance() {
        throw new UnsupportedOperationException("Not implemented");
    }
}
