import typing as tp

import pandas as pd


def male_age(df: pd.DataFrame) -> float:
    """
    Return mean age of survived men, embarked in Southampton with fare > 30
    :param df: dataframe
    :return: mean age
    """

    return df[(df['Sex'] == 'male') & (df['Survived'] == 1) &
              (df['Embarked'] == 'S') & (df['Fare'] > 30) & df['Age'].notnull()]['Age'].mean()


def nan_columns(df: pd.DataFrame) -> tp.Iterable[str]:
    """
    Return list of columns containing nans
    :param df: dataframe
    :return: series of columns
    """
    return df.loc[:, df.isnull().any()].columns


def class_distribution(df: pd.DataFrame) -> pd.Series:
    """
    Return Pclass distrubution
    :param df: dataframe
    :return: series with ratios
    """

    return df['Pclass'].value_counts().sort_index() / len(df['Pclass'])


def families_count(df: pd.DataFrame, k: int) -> int:
    """
    Compute number of families with more than k members
    :param df: dataframe,
    :param k: number of members,
    :return: number of families
    """

    data = df['Name'].str.split(',').str[0].value_counts()
    return len(data[data > k])


def mean_price(df: pd.DataFrame, tickets: tp.Iterable[str]) -> float:
    """
    Return mean price for specific tickets list
    :param df: dataframe,
    :param tickets: list of tickets,
    :return: mean fare for this tickets
    """

    return df.loc[df['Ticket'].isin(tickets)]['Fare'].mean()


def max_size_group(df: pd.DataFrame, columns: list[str]) -> tp.Iterable[tp.Any]:
    """
    For given set of columns compute most common combination of values of these columns
    :param df: dataframe,
    :param columns: columns for grouping,
    :return: list of most common combination
    """

    return tuple(df.groupby(columns).size()
                 .reset_index(name='res')
                 .sort_values(by='res', ascending=False)
                 .iloc[0][columns].values)


def is_lucky(ticket: str) -> bool:
    if ticket.isdigit() and len(ticket) % 2 == 0:
        tail = ticket[len(ticket) // 2 + 1:]
        head = ticket[:len(ticket) // 2]
        return sum(int(i) for i in tail) == sum(int(i) for i in head)
    return False


def dead_lucky(df: pd.DataFrame) -> float:
    """
    Compute dead ratio of passengers with lucky tickets.
    A ticket is considered lucky when it contains an even number of digits in it
    and the sum of the first half of digits equals the sum of the second part of digits
    ex:
    lucky: 123222, 2671, 935755
    not lucky: 123456, 62869, 568290
    :param df: dataframe,
    :return: ratio of dead lucky passengers
    """

    data = df.copy()
    data['IsLucky'] = data['Ticket'].apply(is_lucky)
    return data[data['IsLucky']]['Survived'].mean()
