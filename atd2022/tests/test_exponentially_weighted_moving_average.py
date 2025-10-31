import pandas as pd
from hypothesis import given, settings, strategies as st

from atd2022.forecasters.naive import ExponentiallyWeightedMovingAverage
from tests.strategies import multivariate_timeseries


@settings(max_examples=1, deadline=5000)
@given(
    df=multivariate_timeseries(
        max_columns=2, min_rows=20, max_rows=30, column_type="multi"
    ),
    num_predict=st.integers(min_value=1, max_value=10),
    span=st.integers(min_value=1, max_value=100),
)
def test_historical_forecast_multi(
    df: pd.DataFrame,
    num_predict: int,
    span: int,
) -> None:
    model = ExponentiallyWeightedMovingAverage(span=span)
    train = df[:-num_predict]
    test = df[-num_predict:]
    model = model.fit(train)
    pred = model.predict(test.index)

    assert pred.columns.equals(train.columns)
    assert pred.index.equals(test.index)
    assert pred.notna().all(axis=None)
    assert (pred >= 0).all(axis=None)
