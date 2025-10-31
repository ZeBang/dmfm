import warnings
from typing import Tuple

import pandas as pd
from hypothesis import given, settings
from matplotlib import pyplot as plt

import atd2022.viz

from .strategies import matching_multivariate_timeseries, multivariate_timeseries


@given(
    true_pred=matching_multivariate_timeseries(
        max_columns=3,
        min_rows=2,
        max_rows=20,
    )
)
@settings(max_examples=1, deadline=None)
def test_compare_scalar(true_pred: Tuple[pd.DataFrame, pd.DataFrame]) -> None:
    true, pred = true_pred
    true[true.isna()] = 0
    pred[pred.isna()] = 0

    fig = atd2022.viz.plot_pred_vs_true(true, pred)

    assert isinstance(fig, plt.Figure)


@given(
    true_pred=matching_multivariate_timeseries(
        max_columns=3,
        min_rows=2,
        max_rows=20,
    )
)
@settings(max_examples=1, deadline=None)
def test_compare_list(true_pred: Tuple[pd.DataFrame, pd.DataFrame]) -> None:
    true, pred = true_pred
    true[true.isna()] = 0
    pred[pred.isna()] = 0

    fig = atd2022.viz.plot_pred_vs_true(true, [pred, pred])

    assert isinstance(fig, plt.Figure)


@given(
    true=multivariate_timeseries(
        max_columns=3,
        min_rows=2,
        max_rows=20,
    )
)
@settings(max_examples=1, deadline=None)
def test_plot_behavior(true: pd.DataFrame) -> None:
    true[true.isna()] = 0
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", UserWarning)
        fig = atd2022.viz.plot_behavior(true)
        assert isinstance(fig, plt.Axes)
