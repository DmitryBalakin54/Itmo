#include <iostream>
#include <vector>
#include <queue>

using namespace std;

void buildTree(vector<vector<int>>& adjList, int n) {
    vector<bool> visited(n + 1, false);
    queue<int> q;

    int startNode = 1;
    visited[startNode] = true;
    q.push(startNode);

    while (!q.empty()) {
        int currentNode = q.front();
        q.pop();

        for (int neighbor : adjList[currentNode]) {
            if (!visited[neighbor]) {
                cout << currentNode << " " << neighbor << endl;
                visited[neighbor] = true;
                q.push(neighbor);
            }
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;

    vector<vector<int>> adjList(n + 1);

    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;

        adjList[u].push_back(v);
        adjList[v].push_back(u);
    }

    buildTree(adjList, n);

    return 0;
}
