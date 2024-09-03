import enum


class Status(enum.Enum):
    NEW = 0
    EXTRACTED = 1
    FINISHED = 2


def extract_alphabet(
        graph: dict[str, set[str]]
        ) -> list[str]:
    """
    Extract alphabet from graph
    :param graph: graph with partial order
    :return: alphabet
    """


def build_graph(
        words: list[str]
        ) -> dict[str, set[str]]:
    """
    Build graph from ordered words. Graph should contain all letters from words
    :param words: ordered words
    :return: graph
    """


#########################
# Don't change this code
#########################

def get_alphabet(
        words: list[str]
        ) -> list[str]:
    """
    Extract alphabet from sorted words
    :param words: sorted words
    :return: alphabet
    """
    graph = build_graph(words)
    return extract_alphabet(graph)

#########################
