package ticTacToe;

import java.util.Random;

public class RandomPlayer implements Player {
    private final Random random;

    private final int m;
    private final int n;
    private final int OFFSET;
    public RandomPlayer(final Random random, int m, int n, int offset) {
        this.random = random;
        this.m = m;
        this.n = n;
        this.OFFSET = offset;
    }

    public RandomPlayer() {
        this(3, 3);
    }

    public RandomPlayer(int m, int n) {
        this(new Random(), m, n, 0);
    }

    public RandomPlayer(int m, int n, int offset) {
        this(new Random(), m, n, offset);
    }
    @Override
    public Move move(final Position position, final Cell cell)  {
        while (true) {
            int r = random.nextInt(m + OFFSET);
            int c = random.nextInt(n + OFFSET);
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
