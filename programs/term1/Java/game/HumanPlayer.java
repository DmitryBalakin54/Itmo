package ticTacToe;

import java.io.PrintStream;
import java.util.InputMismatchException;
import java.util.Scanner;

public class HumanPlayer implements Player {
    private final PrintStream out;
    private final Scanner in;

    public HumanPlayer(final PrintStream out, final Scanner in) {
        this.out = out;
        this.in = in;
    }

    public HumanPlayer() {
        this(System.out, new Scanner(System.in));
    }

    @Override
    public Move move(final Position position, final Cell cell) {
        while (true) {
            out.println("Position");
            out.println(position);
            out.println(cell + "'s move");
            out.println("Enter row and column");
            int row;
            int column;
            while (true) {
                try {
                    row = Integer.parseInt(in.next());
                    column = Integer.parseInt(in.next());
                    break;
                } catch (NumberFormatException | InputMismatchException e) {
                    System.out.println("Enter your move again...");
                }
            }
            in.nextLine();
            final Move move = new Move(row, column, cell);
            if (position.isValid(move)) {
                return move;
            }
            System.out.println("invalid move, try again...");
            return move(position, cell);
//            final int row = move.getRow();
//            final int column = move.getColumn();
//            out.println("Move " + move + " is invalid");
        }
    }
}
