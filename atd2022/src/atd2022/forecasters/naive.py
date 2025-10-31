"""Forecasters that use simple heuristics for making predictions."""
from dataclasses import dataclass
from typing import Optional, cast

import numpy as np
import pandas as pd

__all__ = [
    "SampleDistributionForecaster",
    "PredictLastForecaster",
    "PredictMeanForecaster",
    "PredictQuantileForecaster",
    "SimpleMovingAverage",
    "ExponentiallyWeightedMovingAverage",
]


@dataclass
class SampleDistributionForecaster:
    """A forecaster which predicts random observations from the training dataset."""

    def fit(
        self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None
    ) -> "SampleDistributionForecaster":
        """Fit the model.

        Parameters
        ----------
        y : pd.DataFrame
            Target time series training set.
        past_covariates : Optional[pd.DataFrame]
            Additional exogenous data to fit. May be a ``pd.DataFrame`` which has the
            same index but different features than ``y``. In practice,
            ``past_covariates`` is not used by this model.

        Returns
        -------
        self
        """
        self.model = y
        return self

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """Make a multivariate prediction for each entry of x.

        Selects random samples of observations from the training data with replacement.

        Parameters
        ----------
        x : pd.Index
            The times at which future forecasts are made.

        Returns
        -------
        pred : pd.DataFrame
            The multivariate predictions at times in ``x``.
        """
        return cast(pd.DataFrame, self.model.sample(len(x), replace=True)).set_axis(x)


@dataclass
class PredictLastForecaster:
    """A forecaster which predicts the last observation from the training dataset."""

    def fit(
        self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None
    ) -> "PredictLastForecaster":
        """Fit the model.

        Parameters
        ----------
        y : pd.DataFrame
            Target time series training set.
        past_covariates : Optional[pd.DataFrame]
            Additional exogenous data to fit. May be a ``pd.DataFrame`` which has the
            same index but different features than ``y``. In practice,
            ``past_covariates`` is not used by this model.

        Returns
        -------
        self
        """
        self.model = y.iloc[[-1]]
        return self

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """Make a multivariate prediction for each entry of x.

        The prediction for all entries in ``x`` is the last observation of the training
        data. Since the shape of the last observation of the training data is
        ``(1, n_cols)``, this is replicated row-wise by the length of
        ``x`` so that the resulting prediction dataframe of shape ``(len(x), n_cols)``.

        Parameters
        ----------
        x : pd.Index
            The times at which future forecasts are made.

        Returns
        -------
        pred : pd.DataFrame
            The multivariate predictions at times in ``x``.
        """
        pred = self.model.iloc[np.zeros(len(x))]
        pred.index = x
        return pred


@dataclass
class PredictMeanForecaster:
    """A forecaster which predicts the mean of the training dataset."""

    def fit(
        self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None
    ) -> "PredictMeanForecaster":
        """Fit the model.

        Parameters
        ----------
        y : pd.DataFrame
            Target time series training set.
        past_covariates : Optional[pd.DataFrame]
            Additional exogenous data to fit. May be a ``pd.DataFrame`` which has the
            same index but different features than ``y``. In practice,
            ``past_covariates`` is not used by this model.

        Returns
        -------
        self
        """
        self.model = y.mean(axis=0).to_frame().T
        return self

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """Make a multivariate prediction for each entry of ``x``.

        The prediction for all entries in ``x`` is the mean of the training data (for
        each column). Since the shape of the mean is ``(1, n_cols)``, this is
        replicated row-wise by the length of ``x`` so that the resulting prediction
        dataframe of shape ``(len(x), n_cols)``.

        Parameters
        ----------
        x : pd.Index
            The times at which future forecasts are made.

        Returns
        -------
        pred : pd.DataFrame
            The multivariate predictions at times in ``x``.
        """
        pred = self.model.iloc[np.zeros(len(x))]
        pred.index = x
        return pred


@dataclass
class PredictQuantileForecaster:
    """A forecaster which predicts the quantile of the training dataset.

    Parameters
    ----------
    q : float
        Quantile threshold to use between 0 and 1 (default=0.5).
    """

    q: float = 0.5

    def fit(
        self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None
    ) -> "PredictQuantileForecaster":
        """Fit the model.

        Given a threshold ``q`` in ``[0, 1]``, learns the value in each column at which
        ``100*q`` percent of the data is less than the value. If this value occurs
        between data points in the column, the value will be linearly interpolated.

        Parameters
        ----------
        y : pd.DataFrame
            Target time series training set.
        past_covariates : Optional[pd.DataFrame]
            Additional exogenous data to fit. May be a ``pd.DataFrame`` which has the
            same index but different features than ``y``. In practice,
            ``past_covariates`` is not used by this model.

        Returns
        -------
        self
        """
        self.model = y.quantile(self.q, axis=0).to_frame().T
        return self

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """Make a multivariate prediction for each entry of x.

        The prediction for all entries in ``x`` is the quantile of the training data
        (for each column). Since the shape of the quantile is ``(1, n_cols)``, this is
        replicated row-wise by the length of ``x`` so that the resulting prediction
        dataframe of shape ``(len(x), n_cols)``.

        Parameters
        ----------
        x : pd.Index
            The times at which future forecasts are made.

        Returns
        -------
        pred : pd.DataFrame
            The multivariate predictions at times in ``x``.
        """
        pred = self.model.iloc[np.zeros(len(x))]
        pred.index = x
        return pred


@dataclass
class SimpleMovingAverage:
    """A forecaster which predicts the simple moving average for k steps.

    Parameters
    ----------
    k : int
        Number of steps to calculate moving average.
    """

    k: int = 3

    def fit(
        self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None
    ) -> "SimpleMovingAverage":
        """Fit the model.

        Parameters
        ----------
        y : pd.DataFrame
            Target time series training set.
        past_covariates : Optional[pd.DataFrame]
            Additional exogenous data to fit. May be a ``pd.DataFrame`` which has the
            same index but different features than ``y``. In practice,
            ``past_covariates`` is not used by this model.

        Returns
        -------
        self
        """
        if len(y) < self.k:
            raise ValueError(
                f"Insufficient training data (need {self.k} observations)."
            )
        self.model = y.iloc[-self.k :].mean(axis=0).to_frame().T
        return self

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """Make a prediction for each entry of ``x``.

        Parameters
        ----------
        x : pd.Index
            The times at which future forecasts are made.

        Returns
        -------
        pred : pd.DataFrame
            The multivariate predictions at times in ``x``.
        """
        pred = self.model.iloc[np.zeros(len(x))]
        pred.index = x
        return pred


@dataclass
class ExponentiallyWeightedMovingAverage:
    """A forecaster which predicts the exponential moving average.

    Parameters
    ----------
    span : int
        Window size for estimating moving average parameters.
    """

    span: int = 3

    def fit(
        self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None
    ) -> "ExponentiallyWeightedMovingAverage":
        """Fit the model.

        Parameters
        ----------
        y : pd.DataFrame
            Target time series training set.
        past_covariates : Optional[pd.DataFrame]
            Additional exogenous data to fit. May be a ``pd.DataFrame`` which has the
            same index but different features than ``y``. In practice,
            ``past_covariates`` is not used by this model.

        Returns
        -------
        self
        """
        self.model = y.ewm(span=self.span, adjust=False).mean().iloc[[-1]]
        return self

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """Make a prediction for each entry of ``x``.

        Parameters
        ----------
        x : pd.Index
            The times at which future forecasts are made.

        Returns
        -------
        pred : pd.DataFrame
            The multivariate predictions at times in ``x``.
        """
        pred = self.model.iloc[np.zeros(len(x))]
        pred.index = x
        return pred
