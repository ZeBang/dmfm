import pandas as pd
from hypothesis import given, settings
from sktime.forecasting.naive import NaiveForecaster

import atd2022

from .strategies import multivariate_timeseries


@given(
    df=multivariate_timeseries(max_columns=3, min_rows=2, max_rows=20),
)
@settings(max_examples=10)
def test_vectorized_univariate_fit_predict(df: pd.DataFrame) -> None:
    model = atd2022.forecasters.VectorizedUnivariateForecaster(NaiveForecaster)
    results = model.fit(df.iloc[:-1]).predict(df.iloc[[-1]].index)
    assert len(results) == 1
    pd.testing.assert_index_equal(results.columns, df.columns)
