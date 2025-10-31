from typing import List

import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings

import atd2022

from .strategies import multivariate_timeseries

models: List[atd2022.forecasters.Forecaster] = [
    atd2022.forecasters.SampleDistributionForecaster(),
    atd2022.forecasters.PredictLastForecaster(),
    atd2022.forecasters.PredictMeanForecaster(),
    atd2022.forecasters.PredictQuantileForecaster(),
]


@given(
    df=multivariate_timeseries(max_columns=3, min_rows=2, max_rows=20),
)
@settings(max_examples=10)
@pytest.mark.parametrize("model", models, ids=repr)
def test_fit_predict(df: pd.DataFrame, model: atd2022.forecasters.Forecaster) -> None:
    train = df.iloc[:-1]
    test = df.iloc[[-1]]
    pred = model.fit(train).predict(test.index)
    assert pred.columns.equals(train.columns)
    assert pred.index.equals(test.index)
    assert pred.notna().all(axis=None)
    assert (pred >= 0).all(axis=None)


def test_sample_distribution() -> None:
    df = pd.DataFrame({"x": [1, 1]})
    model = atd2022.forecasters.SampleDistributionForecaster()
    expected = [1, 1]
    future_times = df.index[-1:] + 1
    actual = model.fit(df).predict(future_times).values[0]
    assert (actual - expected < 1e-8).all()


def test_predict_last() -> None:
    df = pd.DataFrame({"x": [1, 1, 5]})
    model = atd2022.forecasters.PredictLastForecaster()
    expected = 5
    future_times = df.index[-1:] + 1
    actual = model.fit(df).predict(future_times).values[0]
    assert actual == expected


def test_predict_mean() -> None:
    df = pd.DataFrame({"x": [1, 1, 5]})
    model = atd2022.forecasters.PredictMeanForecaster()
    expected = 7 / 3
    future_times = df.index[-1:] + 1
    actual = model.fit(df).predict(future_times).values[0]
    assert np.abs(actual - expected) < 1e-8


@pytest.mark.parametrize(
    "q,expected",
    (
        (0.25, 1),
        (0.5, 3),
        (0.75, 4),
    ),
)
def test_predict_quantile(q: float, expected: float) -> None:
    model = atd2022.forecasters.PredictQuantileForecaster(q=q)
    df = pd.DataFrame({"x": [1, 1, 3, 4, 5]})
    future_times = df.index[-1:] + 1
    actual = model.fit(df).predict(future_times).values[0]
    assert actual == expected
