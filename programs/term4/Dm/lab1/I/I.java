import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

public class I {

    static final long MOD = 104857601;

    static void readArrays(BufferedReader reader, long[] aArr, long[] cArr, long k) throws IOException{
        StringTokenizer tokenizer = new StringTokenizer(reader.readLine());
        for (int i = 0; i < k; i++) {
            aArr[i] = Long.parseLong(tokenizer.nextToken());
        }

        tokenizer = new StringTokenizer(reader.readLine());
        for (int i = 1; i < k + 1; i++) {
            cArr[i] = MOD - Long.parseLong(tokenizer.nextToken());;
        }
    }

    static long makeA(int i, long k, long[] aArr, long[] cArr) {
        long val = 0;

        for (int j = 1; j < k + 1; j++) {
            val = (val + ((MOD - cArr[j]) * aArr[i - j]) % MOD) % MOD;
        }

        return val;
    }

    static long makeC(int i, long[] cArr) {
        long val = 0;

        for (int j = 0; j < i + 1; j++) {
            val = (val + ((((j & 1) != 0 ? (MOD - cArr[j]) : cArr[j]) * cArr[i - j]) % MOD)) % MOD;
        }

        return val;
    }

    static void changeA(long n, long k, long[] aArr) {
        for (int i = 0; i < k; i++) {
            aArr[i] = aArr[(i << 1) + (int) (n & 1)];
        }
    }

    static long[] newArr(long k, int offset) {
        return new long[(int) (k << 1) + offset];
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer tokenizer = new StringTokenizer(reader.readLine());

        long k = Long.parseLong(tokenizer.nextToken());
        long n = Long.parseLong(tokenizer.nextToken()) - 1;

        long[] aArr = newArr(k, 0);
        long[] cArr = newArr(k, 1);
        readArrays(reader, aArr, cArr, k);
        cArr[0] = 1;

        while (n >= k) {
            long[] v = newArr(k, 1);

            for (int i = (int) k; i < aArr.length; i++) {
                aArr[i] = makeA(i, k, aArr, cArr);
            }

            for (int i = 0; i < v.length;) {
                v[i >> 1] = makeC(i, cArr);
                i += 2;
            }

            cArr = v;
            changeA(n, k, aArr);
            n /= 2;
        }

        System.out.println(aArr[(int) n]);
    }
}
