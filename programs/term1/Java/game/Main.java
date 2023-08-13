package ticTacToe;


import java.io.IOException;
import java.sql.SQLOutput;
import java.util.InputMismatchException;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        mNKGame();
    }


    public static void mNKGame() {
        //final Game game = new Game(true, new HumanPlayer(), new RandomPlayer(m, n, 0));
        final Game game = new Game(true, new HumanPlayer(), new HumanPlayer());
        Scanner scan = new Scanner(System.in);
        Board board = new MnkBoard(scan);
        int result;
        do {
            result = game.play(board);
            System.out.println("Game result: " + result);
            System.out.println(game.total());
            System.out.println();
        } while (game.PlayAgain(board, System.in));
    }
}
