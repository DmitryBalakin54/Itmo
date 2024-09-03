import typing

import pyarrow as pa
import pyarrow.parquet as pq

ValueType = int | list[int] | str | dict[str, str]


def save_rows_to_parquet(rows: list[dict[str, ValueType]], output_filepath: str) -> None:
    """
    Save rows to parquet file.

    :param rows: list of rows containing data.
    :param output_filepath: local filepath for the resulting parquet file.
    :return: None.
    """
    types = {int: pa.int64(), str: pa.string(), list: pa.list_(pa.int64()), dict: pa.map_(pa.string(), pa.string())}
    mask = {}
    for row in rows:
        for key, val in row.items():
            if key not in mask:
                mask[key] = pa.field(key, types[type(val)], False)
            else:
                if mask[key].type != pa.field('', types[type(val)]).type:
                    raise TypeError(f'Field {key} has different types')

        for key in mask:
            if key not in row:
                mask[key] = pa.field(key, mask[key].type, True)

    all_rows = [mask[key] for key in mask]
    p_mask = pa.schema(all_rows)

    data = []
    for row in rows:
        row_vals: list[typing.Any] = []
        for key in p_mask.names:
            if key not in row:
                row_vals.append(None)
            else:
                row_vals.append(row[key])
        data.append(row_vals)

    table = pa.Table.from_pydict({key: [row[i] for row in data] for i, key in enumerate(p_mask.names)},
                                 schema=p_mask)
    pq.write_table(table, output_filepath)
