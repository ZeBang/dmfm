"""Base classes for metrics."""
from typing import Callable, Optional, Union

import numpy as np
import pandas as pd

from .types import MultiOutput


class _FunctionDefinedMetric:
    def __init__(
        self,
        func: Callable,
        multioutput: MultiOutput = "uniform_average",
    ) -> None:
        self._func = func
        self.multioutput = multioutput

    def __call__(
        self,
        y_true: pd.DataFrame,
        y_pred: pd.DataFrame,
        y_train: Optional[pd.DataFrame] = None,
    ) -> Union[pd.Series, float]:
        """Compute metric.

        Parameters
        ----------
        y_true : pd.DataFrame
            Ground truth multivariate timeseries of shape (n_samples, n_series).
        y_pred : pd.DataFrame
            Predicted multivariate timeseries of shape (n_samples, n_series).
        y_train : Optional[pd.DataFrame]
            Training data, potentially used to scale metric value (default=None).

        Returns
        -------
        pd.Series
            Metric values of shape (n_series,).
        """
        _check_index_and_columns(y_true, y_pred)
        vals = self._func(y_true, y_pred)
        vals = (
            vals
            if isinstance(vals, pd.Series)
            else pd.Series(vals, index=y_pred.columns)
        )
        return self.coerce_results(vals)

    def coerce_results(self, vals: pd.Series) -> Union[pd.Series, float]:
        if isinstance(self.multioutput, str) and self.multioutput == "uniform_average":
            return vals.mean()
        elif isinstance(self.multioutput, str) and self.multioutput == "raw_values":
            return vals
        else:
            return (np.asarray(self.multioutput) * vals).sum()


def _check_index_and_columns(true: pd.DataFrame, pred: pd.DataFrame) -> None:
    """Raise error if index or columns of dataframes do not match."""
    if not true.index.equals(pred.index.get_level_values(0)):
        raise ValueError("Top-level index of pred and true must match.")
    if not true.columns.equals(pred.columns):
        raise ValueError("Columns of pred and true must match.")
