"""Objects for adapting univariate forecasters to multivariate forecasters."""
from typing import Any, Dict, Optional, Type

import pandas as pd
from sktime.forecasting.base import ForecastingHorizon

from .types import UnivariateForecaster

__all__ = ["VectorizedUnivariateForecaster"]


class VectorizedUnivariateForecaster:
    """A wrapper to ensemble a univariate forecaster columnwise.

    Although the main focus of the ATD2022 challenge is to develop a multivariate
    forecaster, many canonical time series models are univariate. This class
    provide a wrapper to create an ensemble of models (one for each column of the
    multivariate target) that will collectively behave like a multivariate model.

    As a top-level strategy, this adapter creates a dictionary mapping column keys
    (i.e., (country, event code) pairs) to univariate model instances. Then, in
    ``fit`` or ``predict`` calls, the adapter class dispatches requests to fit a
    particular (country, event code) pair to the appropiate univariate model.

    There are some caveats worth mentioning:

    * This adapater treats each column as separate, which will not fully explo
      not fully exploit the information available to you.
    * The ATD2022 dataset features many columns, so training a model for each (country,
      event code) combination will not be very performant for anything but trivial
      univariate models.
    * Since this class aims to adapt canonical, univariate time series
      models, we have intentionally chosen to not use ``past_covariates`` in
      ``fit`` to support a wider variety of "standard" models.

    For all of the above caveats and more, this class may not directly meet the needs
    of your team. However, you are welcome to copy/paste this code and use it as a
    starting point if you would find elements of it useful.

    See Also
    --------
    :class:`sktime.forecasting.compose.ColumnEnsembleForecaster`
    """

    def __init__(
        self, model: Type[UnivariateForecaster], *args: Any, **kwargs: Any
    ) -> None:
        """Initialize the vectorized model.

        Parameters
        ----------
        model : Type[UnivariateForecaster]
            Constructor for a UnivariateForecaster base model.
        *args : Any
            Base model positional arguments.
        **kwargs : Any
            Base model keyword arguments.
        """

        self._model = model

        self._args = args
        self._kwargs = kwargs
        self.models: Dict[Any, UnivariateForecaster] = {}

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._base_model!r})"

    @property
    def _is_fitted(self) -> bool:
        return bool(self.models)

    @property
    def _base_model(self) -> Any:
        return self._model(*self._args, **self._kwargs)

    def fit(
        self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None
    ) -> "VectorizedUnivariateForecaster":
        """Fit one base model for each column in ``y``.

        Parameters
        ----------
        y : pd.DataFrame
            Target dataframe.
        past_covariates: Optional[pd.DataFrame]
            Optionally, you can provide past covariates. Howevever, they will be
            ignored.

        Returns
        -------
        self
        """
        self.models = {
            col: self._base_model.fit(values) for col, values in y.iteritems()
        }
        self._is_multiindex_columns = isinstance(y.columns, pd.MultiIndex)
        return self

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """Use fitted base models to predict values at each time in ``x``.

        Parameters
        ----------
        x : pd.Index
            Index of dates to predict.

        Returns
        -------
        pred : pd.DataFrame
            Prediction dataframe.
        """
        # self.check_is_fitted()
        pred = pd.concat(
            [
                model.predict(ForecastingHorizon(x, is_relative=False))
                for model_key, model in self.models.items()
            ],
            axis=1,
        )
        pred.columns = (
            pd.MultiIndex.from_tuples(self.models.keys())
            if self._is_multiindex_columns
            else self.models.keys()
        )
        return pred.set_axis(x)
