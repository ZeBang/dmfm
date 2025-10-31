"""Base Forecaster Protocol and types."""
import sys
from typing import Optional

import pandas as pd

if sys.version_info >= (3, 8):
    from typing import Protocol, runtime_checkable
else:
    from typing_extensions import Protocol, runtime_checkable

__all__ = ["Forecaster", "UnivariateForecaster"]


@runtime_checkable
class Forecaster(Protocol):
    """The ATD2022 multivariate forecaster protocol.

    To interoperate with the ``atd2022`` library components, a forecaster must implement
    the :class:`~atd2022.forecasters.types.Forecaster` protocol. This protocol requires
    that you implement at least two, but generally three methods.

    We begin by discussing the optional method, ``__init__``. In
    ``Forecaster.__init__``, users may initialize the object with hyperparamaters
    or static, non-time varying exogenous data. For example, a model that fits a neural
    net with ``k`` hidden layers may accept ``k`` as an argument during model
    initialization. Further, an example of static, non-time varying data might
    include, say, a graph that encodes the pairwise distance between the centroid of
    all countries in the ``atd2022`` dataset or general demographic information that
    describes each country.

    In addition, you must implement two required methods:

        1. :meth:`atd2022.forecasters.types.Forecaster.fit`
        2. :meth:`atd2022.forecasters.types.Forecaster.predict`

    Please refer to each of the above methods help for detailed descriptions of what
    these methods require.
    """

    def fit(
        self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None
    ) -> "Forecaster":
        """Fit the forecaster to the training data.

        In this method, users must write the procedure for performing model
        training/tuning given the available training data. In addition, models may
        accept an optional parameter ``past_covariates``  to include exogenous data at
        model training time. Although the protocol includes ``past_covariates``, this
        optional argument will not be available in practice during the ``atd2022``
        challenge. For a given input training data ``y``, the ``fit`` method is
        responsible for training a model that will eventually be able to make
        predictions for all columns in ``y``.

        Parameters
        ----------
        y : pd.DataFrame
            Target training dataframe with the following properties:

            * The index is a :class:`pandas.PeriodRange` with frequency in weeks.
            * The columns are a :class:`pandas.MultiIndex` with top level equal to
              FIPS country codes and the second level equal to CAMEO event root
              codes.

        past_covariates : Optional[pd.DataFrame]
            Exogenous training dataframe with the following properties:

            * The index is equal to ``y.index``

            In practice, this field will not be used for the ``atd2022`` challenge.
        Returns
        -------
        self
        """

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """Predict future event counts.

        In this method, users must write the procedure for using a trained model
        to make predictions for all observations in the input forecasting horizon.
        Thus, if a model was trained on data that spanned ``n`` columns and
        ``len(x) == m``, then the resulting prediction dataframe should be of size
        ``(m, n)``.

        To make predictions within the :func:`atd2022.backtest.historical_forecast`
        framework, users need not make a model that is truly sensitive to the
        particular dates in ``x``. Instead, it is usually sufficient to ensure that
        the ``predict`` method emits ``len(x)`` predictions.

        Parameters
        ----------
        x : pd.Index
            Dates for which the model should produce forecasts.

        Returns
        -------
        pd.DataFrame
            Dataframe of predictions, with columns matching the columns from the target
            ``y`` provided in the :meth:`atd2022.forecasters.types.Forecaster.fit` and
            index equal to ``x``.
        """


@runtime_checkable
class UnivariateForecaster(Protocol):
    """The univariate forecaster protocol.

    This protocol specifies the requirements for a univariate forecaster to be used in
    :class:`atd2022.forecasters.adapter.VectorizedUnivariateForecaster`. Such a
    forecaster is **not** sufficient for use in the remainder of the ``atd2022``
    library, as it is by definition not natively multivariate.

    Note
    ----
    In general, ``atd2022`` participants will probably not need or want to worry about
    this protocol.
    """

    def fit(self, y: pd.Series) -> "UnivariateForecaster":
        ...

    def predict(self, x: pd.Index) -> pd.Series:
        ...
