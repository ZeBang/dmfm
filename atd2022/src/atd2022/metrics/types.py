"""Types and Protocols related to metrics."""
import sys
from typing import Literal, Optional, Union

import pandas as pd
from numpy.typing import ArrayLike

if sys.version_info >= (3, 8):
    from typing import Protocol, runtime_checkable
else:
    from typing_extensions import Protocol, runtime_checkable

if sys.version_info >= (3, 10):
    from typing import TypeAlias
else:
    from typing_extensions import TypeAlias

__all__ = ["Metric", "MultiOutput"]

MultiOutput: TypeAlias = Union[Literal["raw_values", "uniform_average"], ArrayLike]
"""MultiOutput is inspired from sktime's metric classes. Generally, a metric for a
multivariate time series can be computed in one of three modes:

1. ``"raw_values"`` - Return a metric for each univariate time series. This will return
   a :class:`pd.Series`.
2. ``"uniform_average"`` - Return the unweighted mean of the raw_values as a single
   metric score for the entire series. This will return a ``float``.
3. ``np.typing.ArrayLike`` - Compute a weighted average of the raw_values using the
   weights defined elementwise in the provided :class:`numpy.typing.ArrayLike`. This
   will return a ``float``.
"""


@runtime_checkable
class Metric(Protocol):
    """Metric protocol.

    Metrics should be classes which are configured during initialization. Metrics
    should accept an argument, multioutput, and store it as an attribute. This member
    will be used during metric evaluation to determine the metric behavior for
    multivariate timeseries.

    Metric instances should be callable to implement the metric evaluation procedure.
    See ``__call__`` for more details.
    """

    multioutput: MultiOutput
    # See MultiOutput docstring for description.

    def __call__(
        self,
        y_true: pd.DataFrame,
        y_pred: pd.DataFrame,
        y_train: Optional[pd.DataFrame] = None,
    ) -> Union[float, pd.Series]:
        """Compute the metric.

        Classes implementing ``Metric.__call__`` *must* support the three
        ``multioutput`` options consistent with the
        :const:`atd2022.metrics.types.MultiOutput` type definition.

        Parameters
        ----------
        y_true : pd.DataFrame
        y_pred : pd.DataFrame
        y_train : Optional[pd.DataFrame] = None
            Must accept this argument even if the metric itself does not use it.

        Returns
        -------
        Union[float, pd.Series]
            Return type will be float when `multioutput` is ``"uniform_average"`` or
            and :class:`numpy.typing.ArrayLike`, otherwise return a
            :class:`pandas.Series` where each element corresponds to a column
            (univariate timeseries) from the original dataset.
        """
