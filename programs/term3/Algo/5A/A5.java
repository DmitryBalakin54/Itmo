import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class A5 {

    static class FastReader {
        BufferedReader reader;
        StringTokenizer tokenizer;

        FastReader() {
            reader = new BufferedReader(new InputStreamReader(System.in));
        }

        String next() {
            while (tokenizer == null || !tokenizer.hasMoreTokens()) {
                try {
                    tokenizer = new StringTokenizer(reader.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return tokenizer.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }
    }

    static class NodeInfo {
        int vertex;
        List<Integer> path;

        NodeInfo(int vertex, List<Integer> path) {
            this.vertex = vertex;
            this.path = new ArrayList<>(path);
        }
    }

    static NodeInfo findShortestPath(Map<Integer, List<Integer>> adjacencyList, int start, int end) {
        Queue<NodeInfo> queue = new LinkedList<>();
        Set<Integer> visited = new HashSet<>();

        queue.offer(new NodeInfo(start, Arrays.asList(start)));

        while (!queue.isEmpty()) {
            NodeInfo currentNode = queue.poll();
            int currentVertex = currentNode.vertex;

            if (currentVertex == end) {
                return currentNode;
            }

            if (!visited.contains(currentVertex)) {
                visited.add(currentVertex);

                for (int neighbor : adjacencyList.getOrDefault(currentVertex, Collections.emptyList())) {
                    List<Integer> newPath = new ArrayList<>(currentNode.path);
                    newPath.add(neighbor);
                    queue.offer(new NodeInfo(neighbor, newPath));
                }
            }
        }

        return new NodeInfo(-1, Collections.emptyList());
    }

    public static void main(String[] args) {
        FastReader reader = new FastReader();

        int numVertices = reader.nextInt();
        int numEdges = reader.nextInt();

        Map<Integer, List<Integer>> adjacencyList = new HashMap<>();

        for (int i = 0; i < numEdges; i++) {
            int u = reader.nextInt();
            int v = reader.nextInt();
            adjacencyList.computeIfAbsent(u, key -> new ArrayList<>()).add(v);
        }

        int startVertex = reader.nextInt();
        int endVertex = reader.nextInt();

        NodeInfo result = findShortestPath(adjacencyList, startVertex, endVertex);

        if (result.vertex == -1) {
            System.out.println(-1);
        } else {
            StringBuilder pathString = new StringBuilder();
            pathString.append(result.path.size() - 1).append('\n');
            for (int vertex : result.path) {
                pathString.append(vertex).append(' ');
            }
            System.out.print(pathString);
        }
    }
}
