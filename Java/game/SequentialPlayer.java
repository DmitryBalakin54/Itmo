package ticTacToe;

public class SequentialPlayer implements Player {
    private final int m;
    private final int n;

    SequentialPlayer(int m, int n) {
        this.m = m;
        this.n = n;
    }
    @Override
    public Move move(final Position position, final Cell cell) {
//        Board board = (Board) position;
//        board.makeMove()
        for (int r = 0; r < m; r++) {
            for (int c = 0; c < n; c++) {
                final Move move = new Move(r, c, cell);
                if (position.isValid(move)) {
                    return move;
                }
            }
        }
        return new Move(-1, -1, cell);
        //throw new IllegalStateException("No valid moves");
    }
}
