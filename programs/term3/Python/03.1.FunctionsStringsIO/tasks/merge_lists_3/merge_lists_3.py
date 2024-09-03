import typing as tp
import heapq


def merge(input_streams: tp.Sequence[tp.IO[bytes]], output_stream: tp.IO[bytes]) -> None:
    """
    Merge input_streams in output_stream
    :param input_streams: list of input streams. Contains byte-strings separated by "\n". Nonempty stream ends with "\n"
    :param output_stream: output stream. Contains byte-strings separated by "\n". Nonempty stream ends with "\n"
    :return: None
    """

    k = len(input_streams)
    heap: list[tuple[int, int, bytes]] = []

    for i in range(k):
        element = input_streams[i].readline()
        if len(element) > 1:
            heapq.heappush(heap, (int.from_bytes(element), i, element))
        else:
            k -= 1

    while k > 0:
        el, ind, bytes_str = heapq.heappop(heap)
        output_stream.write(bytes_str)
        new_el = input_streams[ind].readline()
        if len(new_el) == 0:
            k -= 1
        else:
            heapq.heappush(heap, (int.from_bytes(new_el), ind, new_el))
