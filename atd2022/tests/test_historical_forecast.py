import pandas as pd
from hypothesis import given, settings, strategies as st

import atd2022

from .strategies import multivariate_timeseries


@given(
    df=multivariate_timeseries(max_columns=1, min_rows=25, max_rows=30),
    window=st.integers(min_value=1, max_value=10),
    gap=st.integers(min_value=0, max_value=10),
    slide=st.integers(min_value=1, max_value=10),
)
@settings(max_examples=10)
def test_historical_forecast(
    df: pd.DataFrame, window: int, gap: int, slide: int
) -> None:
    num_predict = 1
    model = atd2022.forecasters.SampleDistributionForecaster()
    splitter = atd2022.backtest.Splitter(
        df, window=window, num_predict=num_predict, gap=gap, slide=slide
    )
    results = atd2022.backtest.historical_forecast(model, splitter)

    assert len(results) == len(splitter)
    pd.testing.assert_index_equal(results.columns, splitter.columns)
