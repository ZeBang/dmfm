"""Exploratory data analysis metrics.

These metrics can operate on the raw timeseries values rather than a (true, pred) pair.
"""
from enum import Enum
from typing import Literal, Union, overload

import numpy as np
import pandas as pd

__all__ = ["SeriesBehavior", "adi", "cov", "classify_behavior"]


def adi(y: pd.DataFrame) -> pd.Series:
    """Compute average demand interval (ADI) for each timeseries.

    Average demand interval is the average interval in time periods between two
    non-zero demand periods.

    ADI is a measure of intermittency; the higher it is, the more intermittent the
    series. For example, an ADI of 1.9 means that on average we expect to see a
    period of non-zero demand every 1.9 periods.

    Parameters
    ----------
    y : pd.DataFrame
        Multivariate timeseries of shape (n_samples, n_series).

    Returns
    -------
    pd.Series
        Metric values of shape (n_series,).
    """
    return y.shape[0] / (y > 0).sum(axis=0)


def cov(y: pd.DataFrame) -> pd.Series:
    """Compute the coefficient of variation of a time series.

    Defined as the stdev / mean, if cov^2 is high that means the variability of
    the series is also high.

    Parameters
    ----------
    y : pd.DataFrame
        Multivariate timeseries of shape (n_samples, n_series).

    Returns
    -------
    pd.Series
        Metric values of shape (n_series,).
    """
    return y.std() / y.mean()


class SeriesBehavior(Enum):
    SMOOTH: int = 0
    INTERMITTENT: int = 1
    ERRATIC: int = 2
    LUMPY: int = 3


@overload
def classify_behavior(
    y: pd.DataFrame,
    detailed: Literal[False] = False,
    adi_thresh: float = 1.32,
    cov2_thresh: float = 0.49,
) -> pd.Series:
    ...


@overload
def classify_behavior(
    y: pd.DataFrame,
    detailed: Literal[True],
    adi_thresh: float = 1.32,
    cov2_thresh: float = 0.49,
) -> pd.DataFrame:
    ...


def classify_behavior(
    y: pd.DataFrame,
    detailed: bool = False,
    adi_thresh: float = 1.32,
    cov2_thresh: float = 0.49,
) -> Union[pd.Series, pd.DataFrame]:
    """Determine whether a time series is smooth, intermittent, erratic, or lumpy.

    ::

                _______________________________
                |              |              |
        High    | Intermittent |    Lumpy     |
                |______________|______________|
                |              |              |
        Low     |    Smooth    |   Erratic    |
         ^      |______________|______________|
        ADI/CV^2 ->   Low           High


    Parameters
    ----------
    y : pd.DataFrame
        Multivariate timeseries of shape (n_samples, n_series).
    adi_thresh: float
        Default = 1.32.
    cov2_thresh: float
        Default = 0.49.

    Returns
    -------
    pd.Series
        Series of shape (n_series,) containg SeriesBehavior values.

    References
    ----------
    Syntetos and Boylan, "On the categorization of demand patterns", 2005.
    """
    _adi = adi(y)
    _cov2 = cov(y) ** 2

    # Note: There are some subtle edge-cases below.
    # 1. If a series of length 1, then CoV will be NaN and ADI will be either 1 or ∞
    #    depending on if the value in the series is nonzero or zero, respectively.
    #    Considering these cases in turn, we want:
    #      * (1, NaN) -> SMOOTH, because low ADI and low variance.
    #      * (∞, NaN) -> INTERMITTENT, because high ADI and low variance.
    # 2. Otherwise, if a series is all zeros, then it has mean 0, which means ADI is ∞
    #    and CoV will be NaN. Following the same argument as above, we would want this
    #    case to be classified as INTERMITTENT.
    #
    # With that in mind, the below implementation satisifies this behavior due to a few
    # properties
    # 1. ∞ >= adi_thresh is True, thus correctly labelled as high ADI.
    # 2. NaN >= cov2_thresh is False, and ~False is True, thus low CoV.
    high_adi = _adi >= adi_thresh
    high_cov2 = _cov2 >= cov2_thresh
    label = pd.Series(np.zeros_like(high_adi), index=y.columns)
    label[:] = SeriesBehavior.SMOOTH
    label[high_adi & high_cov2] = SeriesBehavior.LUMPY
    label[high_adi & (~high_cov2)] = SeriesBehavior.INTERMITTENT
    label[(~high_adi) & high_cov2] = SeriesBehavior.ERRATIC
    if detailed:
        return pd.DataFrame({"behavior": label, "adi": _adi, "cov2": _cov2})
    else:
        return label
