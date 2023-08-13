package ticTacToe;

import java.util.Random;

public class BadRandomPlayer implements Player {
    private final Random random;

    private final int m;
    private final int n;
    public BadRandomPlayer(final Random random, int m, int n) {
        this.random = random;
        this.m = m;
        this.n = n;
    }

    public BadRandomPlayer() {
        this(3, 3);
    }

    public BadRandomPlayer(int m, int n) {
        this(new Random(), m, n);
    }

    @Override
    public Move move(final Position position, final Cell cell)  {
        while (true) {
            int r = random.nextInt(m + 2);
            int c = random.nextInt(n + 2);
            final Move move = new Move(r, c, cell);
//            if (position.isValid(move)) {
//                return move;
//            } else {
//                return move(position, cell);
//            }
            return  move;
        }
    }
}
