import dataclasses


@dataclasses.dataclass
class PickleVersion:
    is_new_format: bool
    version: int


def get_pickle_version(data: bytes) -> PickleVersion:
    """
    Returns used protocol version for serialization.

    :param data: serialized object in pickle format.
    :return: protocol version.
    """
    return PickleVersion(True, data[1]) if data[0] == 128 else PickleVersion(False, -1)
