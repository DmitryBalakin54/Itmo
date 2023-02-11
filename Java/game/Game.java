package ticTacToe;


import java.io.InputStream;
import java.util.Scanner;

public class Game {
    private final boolean log;
    private final Player player1, player2;
    private int firstPlayerResult = 0;
    private int secondPlayerResult = 0;
    private int draws = 0;
    public Game(final boolean log, final Player player1, final Player player2) {
        this.log = log;
        this.player1 = player1;
        this.player2 = player2;
    }

    public int play(Board board) {
        int first = -100;
        int second = -100;
        main : while (true) {
            while (true) {
                final int result1 = move(board, player1, 1);
                if (result1 != -1) {
                    first = result1;
                    break main;
                    //return result1;
                }
                if (!board.getNewMove()) {
                    break;
                }
            }
            while (true) {
                final int result2 = move(board, player2, 2);
                if (result2 != -1) {
                    second = result2;
                    break main;
                    //return result2;
                }
                if (!board.getNewMove()) {
                    break;
                }
            }
        }
        int result = first != -100 ? first : second;
        if (result == 1) {
            firstPlayerResult++;
        } else if (result == 2) {
            secondPlayerResult++;
        } else {
            draws++;
        }
        return result;
    }

    public String total() {
        return "Player1 : " + firstPlayerResult + " Player2 : "
                + secondPlayerResult + " Draws : " + draws;
    }

    private void clearTotal() {
        firstPlayerResult = 0;
        secondPlayerResult = 0;
        draws = 0;
    }
    public boolean PlayAgain(Board board, InputStream in) {
        Scanner scan = new Scanner(in);
        System.out.println("Play again Y/N or change settings S");
        String s = scan.nextLine();
        while (!s.equals("Y") && !s.equals("N") && !s.equals("S")) {
            System.out.println("Not correct answer, try again");
            //scan.nextLine();
            s = scan.nextLine();
        }
        if (s.equals("N")) {
            return false;
        } else if (s.equals("S")) {
            board.resize();
            clearTotal();
            return true;
        }
        board.newGame();
        return true;
    }

    private int move(final Board board, final Player player, final int no) {
        final Move move;
        try {
            move = player.move(board.getPosition(), board.getCell());
        } catch (Exception e) {
            log("Player " + no + " lose");
            return 3 - no;
        }
        final Result result = board.makeMove(move);
        log("Player " + no + " move: " + move);
        log("Position:\n" + board);
        if (result == Result.WIN) {
            log("Player " + no + " won");
            return no;
        } else if (result == Result.LOSE) {
            log("Player " + no + " lose");
            return 3 - no;
        } else if (result == Result.DRAW) {
            log("Draw");
            return 0;
        } else if (result == null) {
            return -1;
        } else {
            return -1;
        }
    }

    private void log(final String message) {
        if (log) {
            System.out.println(message);
        }
    }
}
