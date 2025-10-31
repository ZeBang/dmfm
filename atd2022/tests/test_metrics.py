import itertools
from typing import Literal, Tuple, Union

import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
from numpy.typing import ArrayLike
from sktime.performance_metrics.forecasting import MeanAbsoluteScaledError

import atd2022.metrics as atdm

from .strategies import matching_multivariate_timeseries, multivariate_timeseries

METRICS = [
    atdm.Cfe(),
    atdm.Cfe(statistic="max"),
    atdm.Cfe(statistic="min"),
    atdm.Nosp(),
    atdm.Pis(),
    atdm.Spec(),
]


def test_compute_metrics_raises_error_when_not_metric() -> None:
    metrics = [1]
    true = pd.DataFrame()
    pred = pd.DataFrame()
    with pytest.raises(atdm.NotMetricError):
        atdm.compute_metrics(true, [pred], metrics)  # type: ignore [arg-type]


def test_compute_metrics_raises_error_when_columnwise_and_not_raw_values() -> None:
    metrics = [MeanAbsoluteScaledError()]
    true = pd.DataFrame()
    pred = pd.DataFrame()
    with pytest.raises(atdm.InvalidMetricModeError):
        atdm.compute_metrics(true, [pred], metrics, columnwise=True)


def test_compute_metrics_raises_error_when_not_columnwise_and_raw_values() -> None:
    metrics = [MeanAbsoluteScaledError(multioutput="raw_values")]
    true = pd.DataFrame()
    pred = pd.DataFrame()
    with pytest.raises(atdm.InvalidMetricModeError):
        atdm.compute_metrics(true, [pred], metrics, columnwise=False)


@given(
    true_pred=matching_multivariate_timeseries(
        max_columns=3,
        min_rows=2,
        max_rows=20,
    )
)
@settings(max_examples=10)
def test_compute_metrics_columnwise(
    true_pred: Tuple[pd.DataFrame, pd.DataFrame]
) -> None:
    true, pred = true_pred
    true[true.isna()] = 0
    pred[pred.isna()] = 0

    metric_functions = [MeanAbsoluteScaledError(multioutput="raw_values")]

    metrics = atdm.compute_metrics(
        true, [pred], metric_functions, columnwise=True, train=true
    )

    assert isinstance(metrics, pd.DataFrame)
    assert metrics.shape == (1, true.shape[1])


@given(
    true_pred=matching_multivariate_timeseries(
        max_columns=3,
        min_rows=2,
        max_rows=20,
    )
)
@settings(max_examples=10)
def test_compute_metrics(true_pred: Tuple[pd.DataFrame, pd.DataFrame]) -> None:
    true, pred = true_pred
    true[true.isna()] = 0
    pred[pred.isna()] = 0

    metric_functions = [MeanAbsoluteScaledError()]

    metrics = atdm.compute_metrics(
        true, [pred], metric_functions, columnwise=False, train=true
    )

    assert isinstance(metrics, pd.DataFrame)
    assert metrics.shape == (1, 1)


@pytest.mark.parametrize(
    "metric,multioutput",
    itertools.product(METRICS, ("uniform_average", "raw_values")),
    ids=lambda x: x.__class__.__name__ if isinstance(x, atdm.Metric) else str(x),
)
def test_metric(
    metric: atdm.Metric,
    multioutput: Union[Literal["raw_values", "uniform_average"], ArrayLike],
) -> None:
    true = pd.DataFrame(np.random.randint(0, 50, (50, 10)))
    pred = pd.DataFrame(np.random.randint(0, 50, (50, 10)))
    metric.multioutput = multioutput
    metrics = metric(true, pred)

    if multioutput == "uniform_average":
        assert isinstance(metrics, float)
    else:
        assert isinstance(metrics, pd.Series)
        assert metrics.index.equals(true.columns)


@pytest.mark.parametrize("metric", METRICS, ids=lambda x: x.__class__.__name__)
def test_spec_weights(metric: atdm.Metric) -> None:
    true = pd.DataFrame(np.random.randint(0, 50, (50, 10)))
    pred = pd.DataFrame(np.random.randint(0, 50, (50, 10)))
    metric.multioutput = np.arange(10) / np.arange(10).sum()
    metrics = metric(true, pred)

    assert isinstance(metrics, float)


@given(
    df=multivariate_timeseries(min_rows=10, max_rows=20, min_columns=1, max_columns=3),
)
def test_adi(df: pd.DataFrame) -> None:
    result = atdm.adi(df)
    assert isinstance(result, pd.Series)
    assert len(result) == df.shape[1]
    assert (result >= 1).all()


@given(
    df=multivariate_timeseries(min_rows=10, max_rows=20, min_columns=1, max_columns=3),
)
def test_cov(df: pd.DataFrame) -> None:
    result = atdm.cov(df)
    assert isinstance(result, pd.Series)
    assert len(result) == df.shape[1]


@given(
    df=multivariate_timeseries(min_rows=10, max_rows=20, min_columns=1, max_columns=3),
)
def test_classify_behavior_simple(df: pd.DataFrame) -> None:
    result = atdm.classify_behavior(df)
    assert isinstance(result, pd.Series)
    assert len(result) == df.shape[1]
    tmp = result.apply(lambda x: isinstance(x, atdm.SeriesBehavior))
    assert tmp.all()


@pytest.mark.parametrize(
    "df,expected",
    (
        (pd.DataFrame(np.zeros((10, 1))), atdm.SeriesBehavior.INTERMITTENT),
        (pd.DataFrame([[1]]), atdm.SeriesBehavior.SMOOTH),
        (pd.DataFrame([[0]]), atdm.SeriesBehavior.INTERMITTENT),
    ),
    ids=["all zeros", "one nonzero", "one zero"],
)
def test_classify_behavior_edge_cases(
    df: pd.DataFrame, expected: atdm.SeriesBehavior
) -> None:
    result = atdm.classify_behavior(df)
    assert result[0] == expected


@given(
    df=multivariate_timeseries(min_rows=10, max_rows=20, min_columns=1, max_columns=3),
)
def test_classify_behavior_detailed(df: pd.DataFrame) -> None:
    result = atdm.classify_behavior(df, detailed=True)
    assert isinstance(result, pd.DataFrame)
    assert len(result) == df.shape[1]
    tmp = result["behavior"].apply(lambda x: isinstance(x, atdm.SeriesBehavior))
    assert tmp.all()
