import pandas as pd
import pytest
from hypothesis import given, settings

from atd2022.forecasters.naive import SimpleMovingAverage
from tests.strategies import multivariate_timeseries


@settings(max_examples=1, deadline=5000)
@given(
    df=multivariate_timeseries(
        max_columns=2, min_rows=20, max_rows=30, column_type="multi"
    ),
)
def test_historical_forecast_multi(
    df: pd.DataFrame,
) -> None:
    num_predict = 1
    model = SimpleMovingAverage(k=5)
    train = df[:-num_predict]
    test = df[-num_predict:]
    model = model.fit(train)
    pred = model.predict(test.index)

    assert pred.columns.equals(train.columns)
    assert pred.index.equals(test.index)
    assert pred.notna().all(axis=None)
    assert (pred >= 0).all(axis=None)


@given(
    df=multivariate_timeseries(
        max_columns=2, min_rows=20, max_rows=30, column_type="multi"
    ),
)
def test_requires_at_least_k_obs(
    df: pd.DataFrame,
) -> None:
    with pytest.raises(ValueError):
        SimpleMovingAverage(k=40).fit(df)
