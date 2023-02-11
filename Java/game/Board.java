package ticTacToe;

public interface Board {

    boolean getNewMove();

    Position getPosition();
    Cell getCell();
    Result makeMove(Move move);
    void resize();
    void newGame();
}
