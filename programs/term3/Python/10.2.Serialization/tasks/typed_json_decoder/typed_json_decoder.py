import typing as tp
import json

from decimal import Decimal


def decode_typed_json(json_value: str) -> tp.Any:
    """
    Returns deserialized object from json string.
    Checks __custom_key_type__ in object's keys to choose appropriate type.

    :param json_value: serialized object in json format
    :return: deserialized object
    """
    prs = json.loads(json_value)
    fns = {'int': int, 'float': float, 'decimal': Decimal}

    def make(el: tp.Any) -> tp.Any:
        if type(el) is dict:
            f = (lambda x: x)
            if '__custom_key_type__' in el:
                f = fns[el['__custom_key_type__']]
                el.pop('__custom_key_type__')
            new_dct = {}
            for key in el:
                new_dct[f(key)] = make(el[key])
            return new_dct
        elif type(el) is list:
            new_list = []
            for val in el:
                new_list.append(make(val))
            return new_list
        else:
            return el

    return make(prs)
