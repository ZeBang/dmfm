"""Metrics related to Stock-keeping-oriented Prediction Error Costs."""
from dataclasses import dataclass
from functools import partial
from typing import Callable

import numpy as np
from numpy.typing import ArrayLike

from .._base import _FunctionDefinedMetric
from ..types import MultiOutput

__all__ = ["Spec"]


@dataclass
class Spec(_FunctionDefinedMetric):
    """Compute Stock-keeping-oriented Prediction Error Costs (SPEC).

    Parameters
    ----------
    multioutput : Literal["raw_values", "uniform_average"]
        If "raw_values", returns a full set of errors in case of multioutput input.
        If "uniform_average’, errors of all outputs are averaged with uniform weight.
    a1 : opportunity costs weighting parameter
        a1 ∈ [0, ∞]. Default value is 0.75.
    a2 : stock-keeping costs weighting parameter
        a2 ∈ [0, ∞]. Default value is 0.25.
    """

    multioutput: MultiOutput = "uniform_average"
    a1: float = 0.75
    a2: float = 0.25

    @property
    def _func(self) -> Callable[[np.ndarray, np.ndarray], np.ndarray]:
        return partial(spec, a1=self.a1, a2=self.a2)


def spec(
    y_true: ArrayLike, y_pred: ArrayLike, a1: float = 0.75, a2: float = 0.25
) -> np.ndarray:
    """Compute Stock-keeping-oriented Prediction Error Costs (SPEC).

    Parameters
    ----------
    y_true : array-like of shape (n_samples, n_series)
        Ground truth (correct) target values.
    y_pred : array-like of shape (n_samples, n_series)
        Estimated target values.
    a1 : opportunity costs weighting parameter
        a1 ∈ [0, ∞]. Default value is 0.75.
    a2 : stock-keeping costs weighting parameter
        a2 ∈ [0, ∞]. Default value is 0.25.

    Returns
    -------
    loss : np.ndarray
        Array of shape (n_series,) of non-negative floating points (best value is 0.0).

    References
    ----------
    Paper :ref:`https://arxiv.org/abs/2004.10537`
    Adapted from :ref:`https://github.com/DominikMartin/spec_metric`
    """
    y_true = np.asarray(y_true)
    y_pred = np.asarray(y_pred)
    assert len(y_true) > 0 and len(y_pred) > 0
    assert y_true.shape == y_pred.shape

    cum_true = y_true.cumsum(axis=0)
    cum_pred = y_pred.cumsum(axis=0)
    sum_n = np.zeros(y_true.shape[1])
    for t in range(len(y_true)):
        delta1 = cum_true[: t + 1, :] - cum_pred[t, :]
        delta2 = cum_pred[: t + 1, :] - cum_true[t, :]
        opportunity_cost = a1 * np.minimum(y_true[: t + 1, :], delta1)
        stocking_cost = a2 * np.minimum(y_pred[: t + 1, :], delta2)
        penalty = np.maximum(opportunity_cost, stocking_cost)
        penalty = np.maximum(0, penalty)
        time_weight = t - np.arange(t + 1)[:, None] + 1
        sum_t = (penalty * time_weight).sum(axis=0)
        sum_n += sum_t
    return sum_n / len(y_true)
