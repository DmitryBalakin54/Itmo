import pandas as pd


def to_arff(dataframe, filename, relation_name_):
    with open(filename, 'w', encoding='utf-8') as f:
        f.write(f"@RELATION {relation_name_}\n\n")

        for column in dataframe.columns:
            f.write(f"@ATTRIBUTE {column} ")
            if pd.api.types.is_numeric_dtype(dataframe[column]):
                f.write("NUMERIC\n")
            else:
                unique_values = dataframe[column].unique()
                values_str = ','.join(f"'{v}'" for v in unique_values)
                f.write(f"{values_str}\n")

        f.write("\n@DATA\n")

        for index, row in dataframe.iterrows():
            row_str = ','.join(str(value) for value in row)
            f.write(f"{row_str}\n")


data = pd.read_csv('raw_data.tsv', sep='\t', encoding='utf-8')
to_arff(data, 'raw_data.arff', 'xatab_rpg_repacks')
