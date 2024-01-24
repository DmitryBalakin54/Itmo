import java.util.*;

public class E3 {
    static class Edge {
        int start, end;

        public Edge(int start, int end) {
            this.start = start;
            this.end = end;
        }
    }

    static List<Boolean> visited;
    static List<List<Edge>> adjacencyList;
    static int[] entryTime, lowTime, componentId;
    static List<Boolean> isBridge;
    static int time = 0, maxComponentId = 0;

    static void dfs(int vertex, int parent) {
        visited.set(vertex, true);
        entryTime[vertex] = time++;
        lowTime[vertex] = entryTime[vertex];

        for (Edge edge : adjacencyList.get(vertex)) {
            if (!visited.get(edge.start)) {
                dfs(edge.start, edge.end);
            }

            if (edge.end != parent) {
                lowTime[vertex] = Math.min(lowTime[vertex], lowTime[edge.start]);
            }
        }

        if (lowTime[vertex] == entryTime[vertex] && parent != -1) {
            isBridge.set(parent, true);
        }
    }

    static void dfsConnectedComponents(int vertex, int currentComponentId) {
        componentId[vertex] = currentComponentId;

        for (Edge edge : adjacencyList.get(vertex)) {
            if (componentId[edge.start] == 0) {
                if (isBridge.get(edge.end)) {
                    dfsConnectedComponents(edge.start, ++maxComponentId);
                } else {
                    dfsConnectedComponents(edge.start, currentComponentId);
                }
            }
        }
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int numberOfVertices = scanner.nextInt();
        int numberOfEdges = scanner.nextInt();

        adjacencyList = new ArrayList<>(numberOfVertices);
        visited = new ArrayList<>(numberOfVertices);
        entryTime = new int[numberOfVertices];
        lowTime = new int[numberOfVertices];
        isBridge = new ArrayList<>(numberOfEdges);

        for (int i = 0; i < numberOfVertices; ++i) {
            adjacencyList.add(new ArrayList<>());
            visited.add(false);
        }

        for (int i = 0; i < numberOfEdges; ++i) {
            int start = scanner.nextInt() - 1;
            int end = scanner.nextInt() - 1;

            adjacencyList.get(start).add(new Edge(end, i));
            adjacencyList.get(end).add(new Edge(start, i));

            isBridge.add(false);
        }

        for (int i = 0; i < numberOfVertices; ++i) {
            if (!visited.get(i)) {
                dfs(i, -1);
            }
        }

        componentId = new int[numberOfVertices];
        for (int i = 0; i < numberOfVertices; ++i) {
            if (componentId[i] == 0) {
                dfsConnectedComponents(i, ++maxComponentId);
            }
        }

        Map<Integer, Integer> componentIdMap = new HashMap<>();
        int uniqueComponentCount = 0;
        System.out.println(maxComponentId);
        for (int id : componentId) {
            if (componentIdMap.get(id) == null) {
                componentIdMap.put(id, ++uniqueComponentCount);
            }
            System.out.print(componentIdMap.get(id) + " ");
        }

        scanner.close();
    }
}
