import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Stack;
import java.util.StringTokenizer;

public class Main {

    static class Data {
        long w, h, suf;
    }
    static long n, m;
    static long END = 256;
    static long MAX_N = 150000 + 50;
    static long[] s = new long[(int) MAX_N];
    static long[] suff = new long[(int) MAX_N];
    static long[] lcp = new long[(int) MAX_N];
    static long[] classes = new long[(int) MAX_N];
    static long[] classesTemp = new long[(int) MAX_N];
    static long[] suffTemp = new long[(int) MAX_N];
    static long[] pos = new long[(int) MAX_N];

    static long getLetterInd(long ch) {
        return ch;
    }

    static void buildSuffArray() {
        long[] cnt = new long[(int) END];
        for (long i = 0; i < n; ++i)
            ++cnt[(int) getLetterInd(s[(int) i])];
        for (long i = 0; i + 1 < END; ++i)
            cnt[(int) (i + 1)] += cnt[(int) i];

        for (long i = 0; i < n; ++i)
            suff[(int) (--cnt[(int) getLetterInd(s[(int) i])])] = i;
        long count = 1;
        classes[(int) suff[0]] = 0;

        for (long i = 1; i < n; ++i) {
            if (s[(int) suff[(int) i]] != s[(int) suff[(int) (i - 1)]])
                ++count;
            classes[(int) suff[(int) i]] = count - 1;
        }

        for (long l = 0; (1 << l) < n; ++l) {
            long x = 1 << l;
            for (long i = 0; i < n; ++i) {
                suffTemp[(int) i] = suff[(int) i] - x;
                if (suffTemp[(int) i] < 0)
                    suffTemp[(int) i] += n;
            }

            long[] cntTemp = new long[(int) count];
            for (long i = 0; i < n; ++i)
                ++cntTemp[(int) classes[(int) suffTemp[(int) i]]];
            for (long i = 0; i + 1 < count; ++i)
                cntTemp[(int) (i + 1)] += cntTemp[(int) i];
            for (long i = n - 1; i > -1; --i)
                suff[(int) (--cntTemp[(int) classes[(int) suffTemp[(int) i]]])] = suffTemp[(int) i];

            count = 1;
            classes[(int) suff[0]] = 0;
            for (long i = 1; i < n; ++i) {
                if (classes[(int) suff[(int) i]] != classes[(int) suff[(int) (i - 1)]] ||
                        classes[Math.toIntExact((int) (suff[(int) i] + x) % n)] != classes[Math.toIntExact((int) (suff[(int) (i - 1)] + x) % n)])
                    ++count;
                classesTemp[(int) suff[(int) i]] = count - 1;
            }
            classes = classesTemp;
        }
    }

    static void buildLcp() {
        for (long i = 0; i < n; ++i)
            pos[(int) suff[(int) i]] = i;
        long k = 0;
        for (long i = 0; i < n; ++i) {
            if (k > 0)
                --k;
            if (pos[(int) i] == n - 1) {
                lcp[(int) (n - 1)] = -1;
                k = 0;
            } else {
                long cur = suff[(int) (pos[(int) i] + 1)];
                while (Math.max(i + k, cur + k) < n && s[(int) (i + k)] == s[(int) (cur + k)])
                    ++k;
                lcp[(int) pos[(int) i]] = k;
            }
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(in.readLine());
        n = Long.parseLong(st.nextToken());
        m = Long.parseLong(st.nextToken());
        END = m + 10;
        st = new StringTokenizer(in.readLine());
        for (long i = 0; i < n; ++i)
            s[(int) i] = Long.parseLong(st.nextToken());
        s[(int) n] = 0;
        ++n;

        buildSuffArray();
        buildLcp();
        Stack<Data> stack1 = new Stack<>();
        long ansH = n - 1;
        long ansW = 1;
        long ansSuf = -1;

        for (long i = 1; i < n; ++i) {
            long curW = 1;
            while (!stack1.isEmpty() && lcp[(int) i] <= stack1.peek().h) {
                Data temp = stack1.pop();
                curW += temp.w;
                if (curW * temp.h > ansH * ansW) {
                    ansH = temp.h;
                    ansW = curW;
                    ansSuf = temp.suf;
                }
            }

            if (stack1.isEmpty() || lcp[(int) i] > stack1.peek().h) {
                Data temp = new Data();
                temp.w = curW;
                temp.h = lcp[(int) i];
                temp.suf = i;
                stack1.push(temp);
            }
        }
        PrintWriter out = new PrintWriter(System.out);
        out.println(ansH * ansW);
        out.println(ansH);
        if (ansSuf != -1)
            for (long i = 0; i < ansH; ++i)
                out.print(s[(int) (suff[(int) ansSuf] + i)] + " ");
        else
            for (long i = 0; i < n - 1; ++i)
                out.print(s[(int) i] + " ");
        out.close();
    }
}
