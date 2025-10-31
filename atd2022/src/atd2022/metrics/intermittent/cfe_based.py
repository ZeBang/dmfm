"""A collection of metrics derived from cumulative forecast error."""
from dataclasses import dataclass
from typing import Literal

import pandas as pd

from .._base import _FunctionDefinedMetric, _check_index_and_columns
from ..types import MultiOutput

__all__ = ["Cfe", "Nosp", "Pis"]


@dataclass
class Cfe(_FunctionDefinedMetric):
    """Compute cumulative forecast error (CFE).

    Parameters
    ----------
        multioutput : atd2022.metrics.MultiOutput
            "raw_values", returns a full set of errors in case of multioutput input.
            If "uniform_average’, errors of all outputs are averaged with uniform
            weight.
        statistic: Literal["last", "max", "min"]
            If last, get CFE for last time step in period. If max, get maximum.
            If min, get minimum.
    """

    multioutput: MultiOutput = "uniform_average"
    statistic: Literal["last", "max", "min"] = "last"

    def __post_init__(self) -> None:
        self._func = {"last": cfe, "min": cfe_min, "max": cfe_max}[self.statistic]


def all_times_cfe(true: pd.DataFrame, pred: pd.DataFrame) -> pd.DataFrame:
    """Compute cumulative forecast error (CFE) for each time 0...t.

    Parameters
    ----------
    true : pd.DataFrame
        Ground truth multivariate timeseries of shape (n_samples, n_series).
    pred : pd.DataFrame
        Predicted multivariate timeseries of shape (n_samples, n_series).

    Parameters
    ----------
    pd.DataFrame
        CFE values where result[t, s] is CFE at time t for series s.
    """
    _check_index_and_columns(true, pred)
    return (true.values - pred.values).cumsum(axis=0)


def cfe(true: pd.DataFrame, pred: pd.DataFrame) -> pd.Series:
    """Compute cumulative forecast error (CFE) at end of prediction window.

    CFE is defined as the cumulative signed error from time 0 to time t. It represents
    the total amount a forecaster has under- or over-predicted throughout time. In the
    context of demand forecasting, a highly positive CFE would indicate that a store
    had more demand than supply throughout time (resulting in lost opportunities to
    sell) and a highly negative CFE would indicate that a store held too much inventory
    relative to the amount of demand actually observed.

    Plain `cfe` is simply the total cfe at the final time in a prediction window.

    Parameters
    ----------
    true : pd.DataFrame
        Ground truth multivariate timeseries of shape (n_samples, n_series).
    pred : pd.DataFrame
        Predicted multivariate timeseries of shape (n_samples, n_series).

    Returns
    -------
    pd.Series
        Metric values of shape (n_series,).

    See also
    --------
    cfe_min
    cfe_max
    """
    _check_index_and_columns(true, pred)
    return (true.values - pred.values).sum(axis=0)


def cfe_min(true: pd.DataFrame, pred: pd.DataFrame) -> pd.Series:
    """Compute minimum cumulative forecast error (CFE) throughout prediction window.

    CFE is defined as the cumulative signed error from time 0 to time t. It represents
    the total amount a forecaster has under- or over-predicted throughout time. In the
    context of demand forecasting, a highly positive CFE would indicate that a store
    had more demand than supply throughout time (resulting in lost opportunities to
    sell) and a highly negative CFE would indicate that a store held too much inventory
    relative to the amount of demand actually observed.

    `cfe_min` is simply the minimum cfe throughout the prediction window.

    Parameters
    ----------
    true : pd.DataFrame
        Ground truth multivariate timeseries of shape (n_samples, n_series).
    pred : pd.DataFrame
        Predicted multivariate timeseries of shape (n_samples, n_series).

    Returns
    -------
    pd.Series
        Metric values of shape (n_series,).

    See also
    --------
    cfe
    cfe_max
    """
    _check_index_and_columns(true, pred)
    return all_times_cfe(true, pred).min(axis=0)


def cfe_max(true: pd.DataFrame, pred: pd.DataFrame) -> pd.Series:
    """Compute maximum cumulative forecast error (CFE) throughout prediction window.

    CFE is defined as the cumulative signed error from time 0 to time t. It represents
    the total amount a forecaster has under- or over-predicted throughout time. In the
    context of demand forecasting, a highly positive CFE would indicate that a store
    had more demand than supply throughout time (resulting in lost opportunities to
    sell) and a highly negative CFE would indicate that a store held too much inventory
    relative to the amount of demand actually observed.

    `cfe_max` is simply the minimum cfe throughout the prediction window.

    Parameters
    ----------
    true : pd.DataFrame
        Ground truth multivariate timeseries of shape (n_samples, n_series).
    pred : pd.DataFrame
        Predicted multivariate timeseries of shape (n_samples, n_series).

    Returns
    -------
    pd.Series
        Metric values of shape (n_series,).

    See also
    --------
    cfe
    cfe_min
    """
    _check_index_and_columns(true, pred)
    return all_times_cfe(true, pred).max(axis=0)


@dataclass
class Nosp(_FunctionDefinedMetric):
    """Number of shortages as a percentage of periods.

    This metrics is used to detect a forecaster is biased to under or
    over predict.

    Parameters
    ----------
        multioutput : Literal["raw_values", "uniform_average"]
         If "raw_values", returns a full set of errors in case of multioutput input.
         If "uniform_average’, errors of all outputs are averaged with uniform weight.
    """

    multioutput: MultiOutput = "uniform_average"

    def __post_init__(self) -> None:
        self._func = nosp


def nosp(true: pd.DataFrame, pred: pd.DataFrame) -> pd.Series:
    """Number of shortages as a percentage of periods.

    This metrics is used to detect a forecaster is biased to under or
    over predict.

    Parameters
    ----------
    true : pd.DataFrame
        Ground truth multivariate timeseries of shape (n_samples, n_series).
    pred : pd.DataFrame
        Predicted multivariate timeseries of shape (n_samples, n_series).

    Returns
    -------
    pd.Series
        Metric values of shape (n_series,).
    """
    return (all_times_cfe(true, pred) < 0).sum(axis=0) / len(pred)


@dataclass
class Pis(_FunctionDefinedMetric):
    """Periods in stock.

    Parameters
    ----------
        multioutput : Literal["raw_values", "uniform_average"]
         If "raw_values", returns a full set of errors in case of multioutput input.
         If "uniform_average’, errors of all outputs are averaged with uniform weight.
    """

    multioutput: MultiOutput = "uniform_average"

    def __post_init__(self) -> None:
        self._func = pis


def pis(true: pd.DataFrame, pred: pd.DataFrame) -> pd.Series:
    """Periods in stock.

    Parameters
    ----------
    true : pd.DataFrame
        Ground truth multivariate timeseries of shape (n_samples, n_series).
    pred : pd.DataFrame
        Predicted multivariate timeseries of shape (n_samples, n_series).

    Returns
    -------
    pd.Series
        Metric values of shape (n_series,)."""
    _check_index_and_columns(true, pred)
    return -all_times_cfe(true, pred).sum(axis=0)
