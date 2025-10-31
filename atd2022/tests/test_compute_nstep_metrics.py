import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
from sktime.performance_metrics.forecasting import MeanSquaredError, MedianSquaredError

import atd2022
from atd2022.backtest import add_prediction_number

from .strategies import multivariate_timeseries


@given(
    df=multivariate_timeseries(max_columns=3, min_rows=12, max_rows=12),
)
@settings(max_examples=10)
def test_add_prediction_number_valid(df: pd.DataFrame) -> None:
    assert isinstance(add_prediction_number(df, 2), pd.DataFrame)
    assert isinstance(add_prediction_number(df, 3), pd.DataFrame)
    assert isinstance(add_prediction_number(df, 4), pd.DataFrame)
    assert isinstance(add_prediction_number(df, 6), pd.DataFrame)


@given(
    df=multivariate_timeseries(max_columns=3, min_rows=12, max_rows=12),
)
@settings(max_examples=10)
def test_add_prediction_number_invalid(df: pd.DataFrame) -> None:
    with pytest.raises(AssertionError):
        add_prediction_number(df, 5)


@given(
    df=multivariate_timeseries(max_columns=3, min_rows=12, max_rows=12),
)
@settings(max_examples=10)
def test_add_prediction_number_inplace(df: pd.DataFrame) -> None:
    assert add_prediction_number(df, 2, inplace=True) is None


def test_compute_nstep_metrics() -> None:
    """Exercise compute_nstep.

    This test is set up so that if we accidentally right in the wrong col/row major
    order, then the desired values would be scrambled.
    """
    true = pd.DataFrame(np.zeros((4, 1)))
    preds = [pd.DataFrame([[2], [8], [2], [8]])]
    metric_fcns = [
        MeanSquaredError(square_root=True),
        MedianSquaredError(square_root=False),
    ]
    preds[0] = atd2022.backtest.add_prediction_number(preds[0], 2)
    metrics = atd2022.metrics.compute_nstep_metrics(
        true, preds, metric_fcns, train=true, models=["a"]
    )
    assert (metrics.values == np.array([[2, 4], [8, 64]])).all()
