import java.util.*;

public class F9 {
    static class Node {
        char c;
        int par;
        Map<Character, Integer> transition;
        int sufLink;
        boolean isTerminal = false;
        boolean hasTermSuf = false;
        int wordInd = -1;

        Node(char c, int par, int sufLink) {
            this.c = c;
            this.par = par;
            this.transition = new HashMap<>();
            this.sufLink = sufLink;
        }
    }

    static final int ROOT = 0;
    static List<Node> nodes;

    static void addString(String s, int wordInd) {
        int cur = ROOT;
        int ptr = 0;

        while (nodes.get(cur).transition.containsKey(s.charAt(ptr))) {
            cur = nodes.get(cur).transition.get(s.charAt(ptr++));
        }

        for (int i = ptr; i < s.length(); i++) {
            int ind = nodes.size();
            nodes.get(cur).transition.put(s.charAt(i), ind);
            nodes.add(new Node(s.charAt(i), cur, -1));
            cur = ind;
        }

        if (cur != ROOT) {
            nodes.get(cur).isTerminal = nodes.get(cur).hasTermSuf = true;
            nodes.get(cur).wordInd = wordInd;
        }
    }

    static void createSufLinks() {
        Queue<Integer> q = new LinkedList<>();
        for (Map.Entry<Character, Integer> entry : nodes.get(ROOT).transition.entrySet()) {
            q.add(entry.getValue());
        }

        while (!q.isEmpty()) {
            int v = q.poll();

            int p;
            p = nodes.get(v).par;
            p = nodes.get(p).sufLink;

            while (p != -1 && !nodes.get(p).transition.containsKey(nodes.get(v).c)) {
                p = nodes.get(p).sufLink;
            }

            if (p == -1) {
                nodes.get(v).sufLink = ROOT;
            } else {
                nodes.get(v).sufLink = nodes.get(p).transition.get(nodes.get(v).c);
            }

            for (int u : nodes.get(v).transition.values()) {
                q.add(u);
            }
        }
    }

    static void createTerminals() {
        Queue<Integer> q = new LinkedList<>();
        for (Map.Entry<Character, Integer> entry : nodes.get(ROOT).transition.entrySet()) {
            q.add(entry.getValue());
        }

        while (!q.isEmpty()) {
            int v = q.poll();

            nodes.get(v).hasTermSuf = nodes.get(nodes.get(v).sufLink).hasTermSuf || nodes.get(v).hasTermSuf;
            for (int u : nodes.get(v).transition.values()) {
                q.add(u);
            }
        }
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int n = scanner.nextInt();
        nodes = new ArrayList<>();
        nodes.add(new Node('\0', -1, -1));

        List<String> words = new ArrayList<>();
        Map<String, Integer> ans = new HashMap<>();

        for (int i = 0; i < n; i++) {
            String s = scanner.next();
            addString(s, i);
            ans.put(s, 0);
            words.add(s);
        }

        createSufLinks();
        createTerminals();

        String text = scanner.next();

        Map<Integer, Integer> states = new HashMap<>();

        int cur = ROOT;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (nodes.get(cur).transition.containsKey(c)) {
                cur = nodes.get(cur).transition.get(c);
            } else if (cur != ROOT) {
                while (nodes.get(cur).sufLink != -1 && !nodes.get(cur).transition.containsKey(c)) {
                    cur = nodes.get(cur).sufLink;
                }

                if (!nodes.get(cur).transition.containsKey(c)) {
                    cur = ROOT;
                } else {
                    cur = nodes.get(cur).transition.get(c);
                }
            }
            if (cur != ROOT) {
                states.put(cur, states.getOrDefault(cur, 0) + 1);
            }
        }

        for (Map.Entry<Integer, Integer> entry : states.entrySet()) {
            int p = entry.getKey();
            if (!nodes.get(p).hasTermSuf) {
                continue;
            }
            while (true) {
                if (nodes.get(p).isTerminal) {
                    ans.put(words.get(nodes.get(p).wordInd), ans.get(words.get(nodes.get(p).wordInd)) + entry.getValue());
                }
                p = nodes.get(p).sufLink;
                if (p == -1 || !nodes.get(p).hasTermSuf) {
                    break;
                }
            }
        }

        for (String word : words) {
            System.out.println(ans.get(word));
        }
    }
}
