from typing import Any

import pandas as pd
from hypothesis import given

import atd2022

from .strategies import multivariate_timeseries


@given(
    df=multivariate_timeseries(
        max_columns=3, min_rows=2, max_rows=20, column_type="multi"
    )
)
def test_read_write_invertible(df: pd.DataFrame, tmpdir_factory: Any) -> None:
    file = tmpdir_factory.mktemp("data").join("data.csv")
    df.to_csv(file)
    df_from_file = atd2022.io.read_csv(file)
    pd.testing.assert_frame_equal(df, df_from_file)


def test_public_dataset_read() -> None:
    df = atd2022.io.read_csv()
    assert len(df) == 215
