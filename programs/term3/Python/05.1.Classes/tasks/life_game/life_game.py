

class LifeGame(object):
    """
    Class for Game life
    """

    _EMPTY = 0
    _ROCK = 1
    _FISH = 2
    _SHRIMP = 3

    def __init__(self, table: list[list[int]]):
        self._table = table.copy()
        self._lines = len(self._table)
        self._columns = len(self._table[0])

    def _get_neighbors(self, line: int, column: int) -> dict[int, int]:
        res: dict[int, int] = {self._EMPTY: 0,
                               self._ROCK: 0,
                               self._FISH: 0,
                               self._SHRIMP: 0}
        if line != 0 and column != 0:
            res[self._table[line - 1][column - 1]] += 1

        if line != 0:
            res[self._table[line - 1][column]] += 1

        if line != 0 and column != self._columns - 1:
            res[self._table[line - 1][column + 1]] += 1

        if column != 0:
            res[self._table[line][column - 1]] += 1

        if column != self._columns - 1:
            res[self._table[line][column + 1]] += 1

        if line != self._lines - 1 and column != 0:
            res[self._table[line + 1][column - 1]] += 1

        if line != self._lines - 1:
            res[self._table[line + 1][column]] += 1

        if line != self._lines - 1 and column != self._columns - 1:
            res[self._table[line + 1][column + 1]] += 1

        return res

    def _update(self, line: int, column: int) -> int:
        cur = self._table[line][column]

        if cur == self._ROCK:
            return self._ROCK

        neighbors = self._get_neighbors(line, column)

        if cur == self._EMPTY:
            if neighbors[self._FISH] == 3:
                return self._FISH
            elif neighbors[self._SHRIMP] == 3:
                return self._SHRIMP
            else:
                return self._EMPTY
        elif cur == self._FISH:
            if neighbors[self._FISH] < 2:
                return self._EMPTY
            elif neighbors[self._FISH] < 4:
                return self._FISH
            else:
                return self._EMPTY
        else:
            if neighbors[self._SHRIMP] < 2:
                return self._EMPTY
            elif neighbors[self._SHRIMP] < 4:
                return self._SHRIMP
            else:
                return self._EMPTY

    def get_next_generation(self) -> list[list[int]]:
        new_table = [i.copy() for i in self._table]
        for line in range(self._lines):
            for column in range(self._columns):
                new_table[line][column] = self._update(line, column)
        self._table = new_table
        return self._table
