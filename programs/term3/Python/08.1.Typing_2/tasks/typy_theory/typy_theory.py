def problem01() -> dict[int, str]:
    return {5: "операция сложения для None и int не определена",
            7: "a ожидает тип int, получает int | None"}


def problem02() -> dict[int, str]:
    return {5: "операция сложения для object и int не определена"}


def problem03() -> dict[int, str]:
    return {9: "ожидаем tp.Set[int], получаем tp.Set[float]",
            13: "ожидаем tp.Set[int], получаем tp.Set[bool]"}


def problem04() -> dict[int, str]:
    return {9: "ожидаем tp.AbstractSer[int], получаем tp.Set[float]"}


def problem05() -> dict[int, str]:
    return {11: "ожидаем B, имеем A"}


def problem06() -> dict[int, str]:
    return {15: "выражение имеет тип T, базовый класс B определил тип как S."}


def problem07() -> dict[int, str]:
    return {25: "получили tp.Callable[[A], A], ожидали tp.Callable[[A], B]",
            27: "получили tp.Callable[[B], A], ожидали tp.Callable[[A], B]",
            28: "получили tp.Callable[[B], B], ожидали tp.Callable[[A], B]"}


def problem08() -> dict[int, str]:
    return {6: "получили tp.Iterable[str], ожидали tp.Sized",
            18: "получили A, ожидали Iterable[str]",
            24: "получили B, ожидали Iterable[str]"}


def problem09() -> dict[int, str]:
    return {32: "Неподдерживаемый тип правого операнда in для Fooable",
            34: "получили list[<nothing>], ожидали Fooable",
            35: "Невозможно создать экземпляр абстрактного класса A с абстрактным атрибутом foo",
            37: "получили C, ожидали Fooable",
            38: "получили Callable[[int], None], ожидалиFooable"}


def problem10() -> dict[int, str]:
    return {}


