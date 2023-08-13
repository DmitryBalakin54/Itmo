#include <vector>
#include "iostream"

using namespace std;

int main() {
        int n;
        cin >> n;
        int sequence[n];
        for (int i = 0; i < n; i++) {
            cin >> sequence[i];
        }

        int dp[n];
        int last[n];
        for (int i = 0; i < n; i++) {
            dp[i] = 1;
            last[i] = -1;
            for (int j = 0; j < i; j++) {
                if (sequence[j] >= sequence[i] && dp[j] + 1 > dp[i]) {
                    dp[i] = dp[j] + 1;
                    last[i] = j;
                }
            }
        }

        int index = 0;
        int length = dp[0];
        for (int i = 0; i < n; i++) {
            if (dp[i] > length) {
                index = i;
                length = dp[i];
            }
        }

        vector<int> res ;
        while (index != -1) {
            res.push_back(index + 1);
            index = last[index];
        }
        cout << length << endl;
        for (int i = res.size() - 1; i >= 0; i--) {
            cout << res[i] << endl;
        }
    return 0;
}
