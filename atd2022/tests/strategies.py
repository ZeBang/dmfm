import datetime
from typing import Any, List, Tuple

import pandas as pd
from hypothesis import strategies as st
from hypothesis.extra import pandas as stp

from atd2022.cameo import Cameo


def multi_column_names(
    min_columns: int = 1, max_columns: int = 5
) -> st.SearchStrategy[List[Tuple[str, str]]]:
    return st.lists(
        st.tuples(
            st.sampled_from(Cameo.countries.index.to_list()),
            st.sampled_from(Cameo.events.index.to_list()),
        ),
        unique=True,
        min_size=min_columns,
        max_size=max_columns,
    )


def column_names(
    min_columns: int = 1, max_columns: int = 5
) -> st.SearchStrategy[List[str]]:
    return st.lists(
        st.sampled_from(Cameo.countries.index.to_list()),
        unique=True,
        min_size=min_columns,
        max_size=max_columns,
    )


@st.composite
def isoweek_range(draw: Any, min_size: int = 1, max_size: int = 100) -> pd.PeriodIndex:
    start_date = to_isoweek(
        draw(
            st.dates(
                min_value=datetime.date(2009, 1, 1),
                max_value=datetime.date(2010, 12, 31),
            )
        )
    )
    n_periods = draw(st.integers(min_value=min_size, max_value=max_size))
    return pd.period_range(start_date, periods=n_periods, freq="W")


@st.composite
def multivariate_timeseries(
    draw: Any,
    min_rows: int = 1,
    max_rows: int = 100,
    min_columns: int = 1,
    max_columns: int = 5,
    column_type: str = "random",
) -> pd.DataFrame:
    if column_type == "flat":
        names = draw(column_names(min_columns=min_columns, max_columns=max_columns))
    elif column_type == "multi":
        names = draw(
            multi_column_names(min_columns=min_columns, max_columns=max_columns)
        )
    elif column_type == "random":
        is_multicolumn = draw(st.booleans())
        names = (
            draw(multi_column_names(min_columns=min_columns, max_columns=max_columns))
            if is_multicolumn
            else draw(column_names(min_columns=min_columns, max_columns=max_columns))
        )
    else:
        raise ValueError(f"Unsupported {column_type=}")
    columns = stp.columns(names, elements=st.integers(min_value=0, max_value=50))
    return draw(
        stp.data_frames(
            columns=columns,
            rows=None,
            index=isoweek_range(min_size=min_rows, max_size=max_rows),
        )
    )


@st.composite
def matching_multivariate_timeseries(
    draw: Any,
    n: int = 2,
    min_rows: int = 1,
    max_rows: int = 100,
    min_columns: int = 1,
    max_columns: int = 5,
    column_type: str = "random",
) -> Tuple[pd.DataFrame, ...]:
    if n < 1:
        raise ValueError("n must be > 1")
    df = draw(
        multivariate_timeseries(
            min_rows, max_rows, min_columns, max_columns, column_type
        )
    )
    other_dfs = [
        draw(
            multivariate_timeseries(
                min_rows=df.shape[0],
                max_rows=df.shape[0],
                min_columns=df.shape[1],
                max_columns=df.shape[1],
            )
        )
        for _ in range(n - 1)
    ]
    other_dfs = [
        pd.DataFrame(x.values, index=df.index, columns=df.columns) for x in other_dfs
    ]
    return df, *other_dfs


def to_isoweek(date: datetime.date) -> datetime.date:
    return datetime.date.fromisocalendar(*date.isocalendar()[:2], 1)
