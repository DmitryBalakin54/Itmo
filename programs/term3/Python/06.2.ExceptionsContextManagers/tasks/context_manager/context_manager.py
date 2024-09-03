import sys
from contextlib import contextmanager
from typing import Iterator, TextIO, Type


@contextmanager
def supresser(*types_: Type[BaseException]) -> Iterator[None]:
    try:
        yield None
    except BaseException as e:
        if type(e) not in types_:
            raise


@contextmanager
def retyper(type_from: Type[BaseException], type_to: Type[BaseException]) -> Iterator[None]:
    try:
        yield None
    except BaseException as e:
        if type(e) is type_from:
            tmp_args = getattr(e, 'args')
            tmp_traceback = e.__traceback__
            e = type_to()
            setattr(e, 'args', tmp_args)
            e.__traceback__ = tmp_traceback
        raise e


@contextmanager
def dumper(stream: TextIO | None = None) -> Iterator[None]:
    try:
        yield None
    except BaseException as e:
        new_stream = sys.stderr if stream is None else stream
        new_stream.write(e.args[0])
        raise
