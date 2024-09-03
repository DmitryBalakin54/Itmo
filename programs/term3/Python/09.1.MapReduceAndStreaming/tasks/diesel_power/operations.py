import copy
import math
import re
from abc import abstractmethod, ABC
import typing as tp
import string

TRow = dict[str, tp.Any]
TRowsIterable = tp.Iterable[TRow]
TRowsGenerator = tp.Generator[TRow, None, None]


class Operation(ABC):
    @abstractmethod
    def __call__(self, rows: TRowsIterable, *args: tp.Any, **kwargs: tp.Any) -> TRowsGenerator:
        pass


class Read(Operation):
    def __init__(self, filename: str, parser: tp.Callable[[str], TRow]) -> None:
        self.filename = filename
        self.parser = parser

    def __call__(self, *args: tp.Any, **kwargs: tp.Any) -> TRowsGenerator:
        with open(self.filename) as f:
            for line in f:
                yield self.parser(line)


class ReadIterFactory(Operation):
    def __init__(self, name: str) -> None:
        self.name = name

    def __call__(self, *args: tp.Any, **kwargs: tp.Any) -> TRowsGenerator:
        for row in kwargs[self.name]():
            yield row


# Operations


class Mapper(ABC):
    """Base class for mappers"""

    @abstractmethod
    def __call__(self, row: TRow) -> TRowsGenerator:
        """
        :param row: one table row
        """
        pass


class Map(Operation):
    def __init__(self, mapper: Mapper) -> None:
        self.mapper = mapper

    def __call__(self, rows: TRowsIterable, *args: tp.Any, **kwargs: tp.Any) -> TRowsGenerator:
        for row in rows:
            for res in self.mapper(row):
                yield res


class Reducer(ABC):
    """Base class for reducers"""

    @abstractmethod
    def __call__(self, group_key: tuple[str, ...], rows: TRowsIterable) -> TRowsGenerator:
        """
        :param rows: table rows
        """
        pass


class Reduce(Operation):
    def __init__(self, reducer: Reducer, keys: tp.Sequence[str]) -> None:
        self.reducer = reducer
        self.keys = keys

    def __call__(self, rows: TRowsIterable, *args: tp.Any, **kwargs: tp.Any) -> TRowsGenerator:
        tup = tuple(*self.keys) if type(self.keys) is str else tuple(self.keys)
        last: int | None = None
        lst: list[TRow] = []
        for row in rows:
            if last is None:
                last = row[self.keys[0]]

            if last != row[self.keys[0]]:  # or len(lst) > 10:
                if lst:
                    for res in self.reducer(tup, lst):
                        yield res
                last = row[self.keys[0]]
                lst = []
            lst.append(row)
        if lst:
            for res in self.reducer(tup, lst):
                yield res


class Joiner(ABC):
    """Base class for joiners"""

    def __init__(self, suffix_a: str = '_1', suffix_b: str = '_2') -> None:
        self._a_suffix = suffix_a
        self._b_suffix = suffix_b

    @abstractmethod
    def __call__(self, keys: tp.Sequence[str], rows_a: TRowsIterable, rows_b: TRowsIterable) -> TRowsGenerator:
        """
        :param keys: join keys
        :param rows_a: left table rows
        :param rows_b: right table rows
        """
        pass


class Join(Operation):
    def __init__(self, joiner: Joiner, keys: tp.Sequence[str]):
        self.keys = keys
        self.joiner = joiner

    def __call__(self, rows: TRowsIterable, *args: tp.Any, **kwargs: tp.Any) -> TRowsGenerator:
        a_it = iter(rows)
        b_it = iter(args[0])
        key = self.keys[0]

        a_lst: list[TRow] = []
        b_lst: list[TRow] = []
        last = None
        a_flag = True
        b_flag = True
        new_a: None | TRow = None
        new_b: None | TRow = None

        while True:
            if a_flag:
                try:
                    new_a = next(a_it)
                except StopIteration:
                    new_a = None

            if b_flag:
                try:
                    new_b = next(b_it)
                except StopIteration:
                    new_b = None

            if a_flag and b_flag and new_a is None and new_b is None and len(a_lst) == 0 and len(b_lst) == 0:
                break

            if last is None:
                a_flag = True
                b_flag = True
                if new_a is not None and new_b is not None:
                    if new_a[key] < new_b[key]:
                        last = new_a[key]
                    else:
                        last = new_b[key]
                elif new_a is None and new_b is None:
                    break
                elif new_a is None:
                    if new_b is not None:
                        last = new_b[key]
                elif new_b is None:
                    if new_a is not None:
                        last = new_a[key]

            if a_flag and new_a is not None:
                if new_a[key] == last:
                    a_lst.append(new_a)
                else:
                    a_flag = False
            elif new_a is None:
                a_flag = False

            if b_flag and new_b is not None:
                if new_b[key] == last:
                    b_lst.append(new_b)
                else:
                    b_flag = False
            elif new_b is None:
                b_flag = False

            if not a_flag and not b_flag:  # or len(a_lst) + len(b_lst) > 10:
                # print(f'a: {a_lst}')
                # print(f'b: {b_lst}')
                for res in self.joiner(self.keys, a_lst, b_lst):
                    yield res

                last = None
                a_lst = []
                b_lst = []


# Dummy operators


class DummyMapper(Mapper):
    """Yield exactly the row passed"""

    def __call__(self, row: TRow) -> TRowsGenerator:
        yield row


class FirstReducer(Reducer):
    """Yield only first row from passed ones"""

    def __call__(self, group_key: tuple[str, ...], rows: TRowsIterable) -> TRowsGenerator:
        for row in rows:
            yield row
            break


# Mappers


class FilterPunctuation(Mapper):
    """Left only non-punctuation symbols"""

    def __init__(self, column: str):
        """
        :param column: name of column to process
        """
        self.column = column

    def __call__(self, row: TRow) -> TRowsGenerator:
        row[self.column] = row[self.column].translate(str.maketrans("", "", string.punctuation))
        yield row


class LowerCase(Mapper):
    """Replace column value with value in lower case"""

    def __init__(self, column: str):
        """
        :param column: name of column to process
        """
        self.column = column

    @staticmethod
    def _lower_case(txt: str) -> str:
        return txt.lower()

    def __call__(self, row: TRow) -> TRowsGenerator:
        row[self.column] = self._lower_case(row[self.column])
        yield row


class Split(Mapper):
    """Split row on multiple rows by separator"""

    def __init__(self, column: str, separator: str | None = None) -> None:
        """
        :param column: name of column to split
        :param separator: string to separate by
        """
        self.column = column
        self.separator = separator

    @staticmethod
    def _split(line: str, sep: str = "\\s+") -> tp.Generator[str, None, None]:
        if sep == '':
            return (c for c in line)
        else:
            return (i.group(1) for i in re.finditer(f'(?:^|{sep})((?:(?!{sep}).)*)', line))

    def __call__(self, row: TRow) -> TRowsGenerator:
        if self.separator:
            sp = self._split(row[self.column], self.separator)
        else:
            sp = self._split(row[self.column])

        for s in sp:
            yield {key: s if key == self.column else val for key, val in row.items()}


class Product(Mapper):
    """Calculates product of multiple columns"""

    def __init__(self, columns: tp.Sequence[str], result_column: str = 'product') -> None:
        """
        :param columns: column names to product
        :param result_column: column name to save product in
        """
        self.columns = columns
        self.result_column = result_column

    def __call__(self, row: TRow) -> TRowsGenerator:
        row[self.result_column] = math.prod(row[i] for i in self.columns)
        yield row


class Filter(Mapper):
    """Remove records that don't satisfy some condition"""

    def __init__(self, condition: tp.Callable[[TRow], bool]) -> None:
        """
        :param condition: if condition is not true - remove record
        """
        self.condition = condition

    def __call__(self, row: TRow) -> TRowsGenerator:
        if self.condition(row):
            yield row


class Project(Mapper):
    """Leave only mentioned columns"""

    def __init__(self, columns: tp.Sequence[str]) -> None:
        """
        :param columns: names of columns
        """
        self.columns = columns

    def __call__(self, row: TRow) -> TRowsGenerator:
        new_row: TRow = {}

        for col in self.columns:
            new_row[col] = row[col]

        yield new_row


# Reducers

class TopN(Reducer):
    """Calculate top N by value"""

    def __init__(self, column: str, n: int) -> None:
        """
        :param column: column name to get top by
        :param n: number of top values to extract
        """
        self.column_max = column
        self.n = n

    def __call__(self, group_key: tuple[str, ...], rows: TRowsIterable) -> TRowsGenerator:
        lst = [i for i in rows]
        lst.sort(key=lambda el: el[self.column_max])
        cnt = self.n
        for row in reversed(lst):
            if cnt > 0:
                yield row
                cnt -= 1
            else:
                break


class TermFrequency(Reducer):
    """Calculate frequency of values in column"""

    def __init__(self, words_column: str, result_column: str = 'tf') -> None:
        """
        :param words_column: name for column with words
        :param result_column: name for result column
        """
        self.words_column = words_column
        self.result_column = result_column

    def __call__(self, group_key: tuple[str, ...], rows: TRowsIterable) -> TRowsGenerator:
        cnt = 0
        dct: dict[str, TRow] = {}
        for row in rows:
            cnt += 1
            if row[self.words_column] in dct:
                dct[row[self.words_column]][self.result_column] += 1.0
            else:
                dct[row[self.words_column]] = {group_key[0]: row[group_key[0]],
                                               self.words_column: row[self.words_column],
                                               self.result_column: 1.0}

        for r in dct.values():
            r[self.result_column] /= cnt
            yield r


class Count(Reducer):
    """
    Count records by key
    Example for group_key=('a',) and column='d'
        {'a': 1, 'b': 5, 'c': 2}
        {'a': 1, 'b': 6, 'c': 1}
        =>
        {'a': 1, 'd': 2}
    """

    def __init__(self, column: str) -> None:
        """
        :param column: name for result column
        """
        self.column = column

    def __call__(self, group_key: tuple[str, ...], rows: TRowsIterable) -> TRowsGenerator:
        cnt = 0
        name = group_key[0]
        val = None
        for row in rows:
            cnt += 1
            val = row[name]

        yield {self.column: cnt, name: val}


class Sum(Reducer):
    """
    Sum values aggregated by key
    Example for key=('a',) and column='b'
        {'a': 1, 'b': 2, 'c': 4}
        {'a': 1, 'b': 3, 'c': 5}
        =>
        {'a': 1, 'b': 5}
    """

    def __init__(self, column: str) -> None:
        """
        :param column: name for sum column
        """
        self.column = column

    def __call__(self, group_key: tuple[str, ...], rows: TRowsIterable) -> TRowsGenerator:
        sm = 0
        name = group_key[0]
        val = None
        for row in rows:
            val = row[name]
            sm += row[self.column]

        yield {name: val, self.column: sm}


# Joiners


class InnerJoiner(Joiner):
    """Join with inner strategy"""

    def __call__(self, keys: tp.Sequence[str], rows_a: TRowsIterable, rows_b: TRowsIterable) -> TRowsGenerator:
        for a in rows_a:
            new_b = copy.deepcopy(rows_b)
            for b in new_b:
                new_row = {key: val for key, val in a.items()}
                for key, val in b.items():
                    if key in new_row and val != new_row[key]:
                        v = new_row.pop(key)
                        new_row[key + self._a_suffix] = v
                        new_row[key + self._b_suffix] = val
                    else:
                        new_row[key] = val
                yield new_row


class OuterJoiner(Joiner):
    """Join with outer strategy"""

    def __call__(self, keys: tp.Sequence[str], rows_a: TRowsIterable, rows_b: TRowsIterable) -> TRowsGenerator:
        new_b = copy.deepcopy(rows_b)
        flag_b = True
        for a in rows_a:
            flag_b = False
            flag_a = True
            new_b = copy.deepcopy(rows_b)
            for b in new_b:
                flag_a = False
                new_row = {key: val for key, val in a.items()}
                for key, val in b.items():
                    if key in new_row and val != new_row[key]:
                        v = new_row.pop(key)
                        new_row[key + self._a_suffix] = v
                        new_row[key + self._b_suffix] = val
                    else:
                        new_row[key] = val
                yield new_row

            if flag_a:
                yield a

        if flag_b:
            for b in new_b:
                yield b


class LeftJoiner(Joiner):
    """Join with left strategy"""

    def __call__(self, keys: tp.Sequence[str], rows_a: TRowsIterable, rows_b: TRowsIterable) -> TRowsGenerator:
        new_b = copy.deepcopy(rows_b)
        for a in rows_a:
            flag_a = True
            new_b = copy.deepcopy(rows_b)
            for b in new_b:
                flag_a = False
                new_row = {key: val for key, val in a.items()}
                for key, val in b.items():
                    if key in new_row and val != new_row[key]:
                        v = new_row.pop(key)
                        new_row[key + self._a_suffix] = v
                        new_row[key + self._b_suffix] = val
                    else:
                        new_row[key] = val
                yield new_row

            if flag_a:
                yield a


class RightJoiner(Joiner):
    """Join with right strategy"""

    def __call__(self, keys: tp.Sequence[str], rows_a: TRowsIterable, rows_b: TRowsIterable) -> TRowsGenerator:
        new_b = copy.deepcopy(rows_b)
        flag_b = True
        for a in rows_a:
            flag_b = False
            new_b = copy.deepcopy(rows_b)
            for b in new_b:
                new_row = {key: val for key, val in a.items()}
                for key, val in b.items():
                    if key in new_row and val != new_row[key]:
                        v = new_row.pop(key)
                        new_row[key + self._a_suffix] = v
                        new_row[key + self._b_suffix] = val
                    else:
                        new_row[key] = val
                yield new_row

        if flag_b:
            for b in new_b:
                yield b
