package ticTacToe;

import java.io.InputStream;
import java.util.Arrays;
import java.util.Map;
import java.util.Scanner;

public class MnkBoard implements Board, Position {

    private static final Map<Cell, Character> SYMBOLS = Map.of(
            Cell.X, 'X',
            Cell.O, 'O',
            Cell.E, '.'
    );

    private int k;
    private int n;
    private int m;
    private int counter;

    private boolean newMove;

    private  Cell[][] cells;
    private Cell turn;

    Scanner in;


    public MnkBoard(final Scanner in) {
        this.in = in;
        resize();
    }

    private void enterMNK(Scanner scan) {
        System.out.println("Enter m, n. k");
        int m;
        int n;
        int k;
        while (true) {
            try {
                m = Integer.parseInt(scan.next());
                n = Integer.parseInt(scan.next());
                k = Integer.parseInt(scan.next());
                if (k > m || k > n) {
                    System.out.println("k > m or k > n. Enter m, n, k again...");
                    continue;
                }
                break;
            } catch (NumberFormatException e) {
                System.out.println("Enter m, n, k again...");
                scan.nextLine();
            }
        }
        this.k = k;
        this.n = n;
        this.m = m;
    }
    public boolean getNewMove() {
        return newMove;
    }
    @Override
    public Position getPosition() { return this; }

    @Override
    public Cell getCell() { return turn; }

    public Result makeMove(final Move move) {
        newMove = false;
        if (!isValid(move)) {
            return Result.LOSE;
        }

        cells[move.getRow()][move.getColumn()] = move.getValue();



        int i = move.getRow();
        int j = move.getColumn();
        if (checkDown(i, j) || checkLeftDown(i, j) ||
                checkRight(i, j) || checkRightDown(i, j))
        {
           return Result.WIN;
        }
        counter++;
        if (counter == m * n) {
            return Result.DRAW;
        }

        if (!newMove) {
            turn = turn == Cell.X ? Cell.O : Cell.X;
        }
        return Result.UNKNOWN;
    }

    @Override
    public void resize() {
        enterMNK(in);
        this.cells = new Cell[m][n];
        for (Cell[] row : cells) {
            Arrays.fill(row, Cell.E);
        }
        turn = Cell.X;
        counter = 0;
    }

    @Override
    public void newGame() {
        this.cells = new Cell[m][n];
        for (Cell[] row : cells) {
            Arrays.fill(row, Cell.E);
        }
        turn = Cell.X;
        counter = 0;
    }


    public boolean checkRightDown(int row, int column) {
        int sequence = 1;
        int saveRow = row;
        int saveColumn = column;
        while (++row < m && ++column < n) {
            if (cells[row][column] == turn) {
                sequence++;
            } else {
                break;
            }
        }
        row = saveRow;
        column  = saveColumn;
        while(--row >=0 && --column >= 0) {
            if (cells[row][column] == turn) {
                sequence++;
            } else {
                break;
            }
        }
        newMove = sequence >= 4 || newMove;
        return sequence >= k;
    }

    public boolean checkRight(int row, int column) {
        int sequence = 1;
        int saveColumn = column;
        while (++column < n) {
            if (cells[row][column] == turn) {
                sequence++;
            } else {
                break;
            }
        }
        column = saveColumn;
        while (--column >= 0) {
            if (cells[row][column] == turn) {
                sequence++;
            } else {
                break;
            }
        }
        newMove = sequence >= 4 || newMove;
        return sequence >= k;
    }

    public boolean checkLeftDown(int row, int column) {
        int sequence = 1;
        int saveRow = row;
        int saveColumn = column;
        while (++row < m && --column > -1) {
            if (cells[row][column] == turn) {
                sequence++;
            } else {
                break;
            }
        }
        row = saveRow;
        column = saveColumn;
        while (--row >= 0 && ++column < n) {
            if (cells[row][column] == turn) {
                sequence++;
            } else {
                break;
            }
        }
        newMove = sequence >= 4 || newMove;
        return sequence >= k;
    }

    public boolean checkDown(int row, int column) {
        int sequence = 1;
        int saveRow = row;
        while (++row < m) {
            if (cells[row][column] == turn) {
                sequence++;
            } else {
                break;
            }
        }
        row = saveRow;
        while (--row >= 0) {
            if (cells[row][column] == turn) {
                sequence++;
            } else {
                break;
            }
        }
        newMove = sequence >= 4 || newMove;
        return sequence >= k;
    }
    @Override
    public boolean isValid(Move move) {
        return  0 <= move.getRow() && move.getRow() < m
                && 0 <= move.getColumn() && move.getColumn() < n
                && cells[move.getRow()][move.getColumn()] == Cell.E
                && turn == getCell();
    }

    @Override
    public Cell getCell(int r, int c) {
        return cells[r][c];
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("");
        for (int i = 0; i < n; i++) {
            //sb.append(i).append(" ");
        }
        for (int r = 0; r < m; r++) {
            // sb.append(r);
            for (int c = 0; c < m; c++) {
                sb.append(" ").append(SYMBOLS.get(cells[r][c]));
            }
            if (r == m - 1) {
                break;
            }
            sb.append("\n");
        }
        return  sb.toString();
    }
}
