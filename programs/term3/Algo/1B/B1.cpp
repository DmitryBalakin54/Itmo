#include <iostream>
#include <vector>

using namespace std;

class Graph {
private:
    int n;
    vector<vector<int>> adjList;

public:
    Graph(int vertices) {
        n = vertices;
        adjList.resize(n + 1);
    }

    void addEdge(int u, int v) {
        adjList[u].push_back(v);
        adjList[v].push_back(u);
    }

    vector<vector<int>> findConnectedComponents() {
        vector<bool> visited(n + 1, false);
        vector<vector<int>> components;

        for (int i = 1; i <= n; i++) {
            if (!visited[i]) {
                vector<int> component;
                dfs(i, visited, component);
                components.push_back(component);
            }
        }

        return components;
    }

private:
    void dfs(int vertex, vector<bool>& visited, vector<int>& component) {
        visited[vertex] = true;
        component.push_back(vertex);

        for (int neighbor : adjList[vertex]) {
            if (!visited[neighbor]) {
                dfs(neighbor, visited, component);
            }
        }
    }
};

int main() {
    int n, m;
    cin >> n >> m;

    Graph myGraph(n);

    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        myGraph.addEdge(u, v);
    }

    vector<vector<int>> components = myGraph.findConnectedComponents();

    cout << components.size() << endl;

    for (const vector<int>& component : components) {
        cout << component.size() << endl;
        for (int vertex : component) {
            cout << vertex << " ";
        }
        cout << endl;
    }

    return 0;
}
