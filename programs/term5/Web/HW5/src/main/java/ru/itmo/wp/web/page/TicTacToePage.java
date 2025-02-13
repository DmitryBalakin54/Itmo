package ru.itmo.wp.web.page;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class TicTacToePage {

    private static final String STATE = "state";
    private static final String NEW_GAME = "newGame";
    private static final String ACTION = "action";

    public static class State {

        public static final String RUNNING = "RUNNING";

        private final int size = 3;
        private final String[][] cells = new String[size][size];
        private boolean crossesMove = true;
        private String phase = RUNNING;

        public State() {
            for (int i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    cells[i][j] = "";
                }
            }
        }

        public int getSize() {
            return size;
        }

        public String[][] getCells() {
            return cells;
        }

        public boolean isCrossesMove() {
            return crossesMove;
        }

        public String getPhase() {
            return phase;
        }

        public void makeMove(int row, int col) {
            if (phase.equals(RUNNING) && cells[row][col].isEmpty()) {
                cells[row][col] = crossesMove ? "X" : "O";
                if (checkWin(row, col)) {
                    phase = crossesMove ? "WON_X" : "WON_O";
                } else if (isDraw()) {
                    phase = "DRAW";
                } else {
                    crossesMove = !crossesMove;
                }
            }
        }

        private boolean checkWin(int row, int col) {
            return checkRow(row) || checkColumn(col) || checkDiagonals(row, col);
        }

        private boolean checkRow(int row) {
            for (int i = 1; i < size; i++) {
                if (!cells[row][i].equals(cells[row][0])) {
                    return false;
                }
            }
            return !cells[row][0].isEmpty();
        }

        private boolean checkColumn(int col) {
            for (int i = 1; i < size; i++) {
                if (!cells[i][col].equals(cells[0][col])) {
                    return false;
                }
            }

            return !cells[0][col].isEmpty();
        }

        private boolean checkDiagonals(int row, int col) {
            if (Math.abs(row - col) != 0 && row + col != size - 1) {
                return false;
            }

            boolean mainDiagonal = !cells[0][0].isEmpty();
            boolean otherDiagonal = !cells[0][size - 1].isEmpty();
            for (int i = 1; i < size; i++) {
                mainDiagonal &= cells[i][i].equals(cells[0][0]);
                otherDiagonal &= cells[i][size - i - 1].equals(cells[0][size - 1]);
            }
            return mainDiagonal || otherDiagonal;
        }

        private boolean isDraw() {
            for (int i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    if (cells[i][j].isEmpty()) {
                        return false;
                    }
                }
            }
            return true;
        }
    }

    public void action(HttpServletRequest request, Map<String, Object> view) {
        State state = (State) request.getSession().getAttribute(STATE);
        if (state == null || NEW_GAME.equals(request.getParameter(ACTION))) {
            state = new State();
            request.getSession().setAttribute(STATE, state);
        }

        view.put(STATE, state);
    }

    public void newGame(HttpServletRequest request, Map<String, Object> view) {
        State state = new State();
        request.getSession().setAttribute(STATE, state);
        view.put(STATE, state);
    }

    public void onMove(HttpServletRequest request, Map<String, Object> view) {
        State state = (State) request.getSession().getAttribute(STATE);
        if (state != null && state.getPhase().equals(State.RUNNING)) {
            request.getParameterMap().forEach((name, value) -> {
                if (name.startsWith("cell_")) {
                    int row = Character.getNumericValue(name.charAt(5));
                    int col = Character.getNumericValue(name.charAt(6));
                    state.makeMove(row, col);
                }
            });
        }

        view.put(STATE, state);
    }
}
