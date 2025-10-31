"""Objects for historical forecasting and backtesting."""
from dataclasses import dataclass, field
from typing import (
    TYPE_CHECKING,
    Generic,
    Iterator,
    Literal,
    Optional,
    Tuple,
    TypeVar,
    Union,
    cast,
    overload,
)

import numpy as np
import pandas as pd
from tqdm.auto import tqdm

from .forecasters import Forecaster

__all__ = ["add_prediction_number", "historical_forecast", "Splitter"]


PC = TypeVar("PC", pd.DataFrame, None)

INDEX_LEVEL1_NAME = "Prediction #"


@dataclass(init=False)
class Splitter(Generic[PC]):
    """Implements a rolling or expanding window train/test dataset splitter.

    This object supports a variety of methods for spliting data (and, if provided,
    exogenous data) into training and testing partitions. The training window may
    be a fixed size (if ``expanding=False``) or expanding window (if
    ``expanding=True``) with the minimum size of the training window equal to the
    ``window`` argument. The testing window will be a fixed size equal to the
    ``num_predict`` argument. The number of observations between the last observation
    in the training window and the first observation of the test window will be equal
    to the ``gap`` argument.

    To illustrate, we can conduct a historical backtest where we use 10 weeks of
    training data to make a single prediction four weeks into the future, we can use
    the paramaters:

    * ``window = 10``
    * ``num_predict = 1``
    * ``gap = 4``

    The user can set the number of observations to step forward between the beginning
    of one split and the next with the ``slide`` parameter. For example, if the current
    first observation of the test dataset was the ``k`` th observation of the dataset,
    then the next test split would begin at ``k + slide``.

    This object supports a variety of convient features:

        * Iterable - ``for train, test, past_covariates in splitter`` to iterate over
          splits.
        * Sized - ``len(splitter)`` to get the number of train/test splits.
        * Indexable - ``splitter[k]`` to get the ``k`` th split (assuming ``k`` is
          between ``0`` and ``len(splitter)``).

    Parameters
    ----------
    y : pd.DataFrame
        Input data.
    window : int
        The minimum size of the training window. If ``expanding`` is ``False``, then
        this will be exact number of training observations.
    num_predict : int
        The size of the testing window (i.e., the number of observations to forecast).
    gap : int
        The number of observations strictly between the end of the training window and
        the beginning of the testing window.
    slide : int
        The number of observations to step forward between the start of each training
        window.
    past_covariates : Optional[pd.DataFrame]
        Additional exogenous data to be split into sliding windows. May be a
        ``pd.DataFrame`` which has the same index as ``y`` but different features than
        ``y``.
    expanding: bool
        If ``True``, allow the training window to expand in size. Otherwise, the number
        of observations in the ``train`` and ``past_covariates`` components of a split
        will be constant (equal to ``window``).

    Example
    -------
    >>> # Generate random dataframe containing 24 rows and 2 columns.
    >>> # The values of the dataframe increase from 1 to 24 along the rows.
    >>> # The index of the dataframe starts in 2022-05-01 and increases monthly
    >>> idx = pd.date_range(start = "2022-05-01", periods = 24, freq = "MS")
    >>> data = np.cumsum(np.ones((24, 2), dtype=int), axis=0)
    >>> y = pd.DataFrame(data, idx, columns=["a", "b"])
    >>> # Set parameters to generate a splitter.
    >>> window = 6  # 6 month training window
    >>> num_predict = 1  # Make 1 prediction each time
    >>> gap = 0  # Seperate the train/test splits by 0 observations (no gap)
    >>> slide = 2  # Slide forward two months each time.
    >>> # Create a dataset `Splitter` object for generating train/test splits
    >>> splitter = Splitter(y, window, num_predict, gap, slide)
    >>> for train, test, _ in splitter:
    ...     print("train:")
    ...     print(train)
    ...     print("test:")
    ...     print(test)
    ...     break
    train:
                a  b
    2022-05-01  1  1
    2022-06-01  2  2
    2022-07-01  3  3
    2022-08-01  4  4
    2022-09-01  5  5
    2022-10-01  6  6
    test:
                a  b
    2022-11-01  7  7
    """

    y: pd.DataFrame = field(repr=False)
    window: int
    num_predict: int
    gap: int
    slide: int
    past_covariates: Optional[pd.DataFrame] = field(repr=False)
    expanding: bool = False

    # Define several overloads for __init__ so that the type system can the type of
    # past_covariates in other functions.

    @overload
    def __init__(
        self: "Splitter[None]",
        y: pd.DataFrame,
        window: int,
        num_predict: int,
        gap: int,
        slide: int,
        past_covariates: None = None,
        expanding: bool = False,
    ) -> None:
        ...

    @overload
    def __init__(
        self: "Splitter[pd.DataFrame]",
        y: pd.DataFrame,
        window: int,
        num_predict: int,
        gap: int,
        slide: int,
        past_covariates: pd.DataFrame,
        expanding: bool = False,
    ) -> None:
        ...

    def __init__(
        self,
        y: pd.DataFrame,
        window: int,
        num_predict: int,
        gap: int,
        slide: int,
        past_covariates: Optional[pd.DataFrame] = None,
        expanding: bool = False,
    ) -> None:
        self.y = y
        self.window = window
        self.num_predict = num_predict
        self.gap = gap
        self.slide = slide
        self.past_covariates = past_covariates
        self.expanding = expanding

        if self.past_covariates is not None and not self.index.equals(
            self.past_covariates.index
        ):
            raise ValueError(
                "The index of y and past_covariates must be exactly equal."
            )

    def __len__(self) -> int:
        """Get the number of splits in the :class:`~atd2022.backtest.Splitter`."""
        len_ = (
            len(self.y) - (self.window + self.gap + self.num_predict)
        ) // self.slide + 1
        return max(0, int(len_))

    def __getitem__(self, key: int) -> Tuple[pd.DataFrame, pd.DataFrame, PC]:
        """Get a split by index."""
        if key not in range(len(self)):
            raise IndexError("Index out of bounds.")

        shift = key * self.slide
        train_idx = (shift, self.window + shift)
        test_idx = (
            self.window + self.gap + shift,
            self.window + self.gap + self.num_predict + shift,
        )
        train_start = 0 if self.expanding else train_idx[0]
        return (
            self.y.iloc[train_start : train_idx[1]],
            self.y.iloc[test_idx[0] : test_idx[1]],
            cast(
                PC,
                None
                if self.past_covariates is None
                else self.past_covariates.iloc[train_start : train_idx[1]],
            ),
        )

    if TYPE_CHECKING:

        def __iter__(
            self,
        ) -> Iterator[Tuple[pd.DataFrame, pd.DataFrame, PC]]:
            """Iterate over the splitter."""
            for i in range(len(self)):
                yield self[i]

    @property
    def columns(self) -> pd.Index:
        """Columns of the target dataframe."""
        return self.y.columns

    @property
    def index(self) -> pd.Index:
        """Index of the target dataframe."""
        return self.y.index


def historical_forecast(
    model: Forecaster, splitter: Splitter, verbose: bool = False
) -> pd.DataFrame:
    """Conduct historical forecast given a Forecaster and Splitter.

    The historical forecast function executes the following process:

    1. for each ``train``/``test``/``past_covariates`` split in the ``splitter``
        * ``fit`` the model, using ``train`` (and ``past_covariates``, if available)
        * Use the fitted model ``predict`` the values of the target time series
          at the times in ``test.index``.
    2. Augment the output dataframe with additional index-level ``Prediction #`` to
       disambiguate predictions that map to the same period.

    Parameters
    ----------
    model : Forecaster
        Model to use for historical forecasting.
    splitter : Splitter
        Dataset splitter to use for partitioning train/test data.
    verbose : bool
        If ``True``, show a progress bar.

    Returns
    -------
    pd.DataFrame
        Dataframe of predictions.
    """
    if len(splitter) == 0:
        raise ValueError("Historical forecasts require a splitter of nonzero length.")

    _splits = (
        tqdm(splitter, total=len(splitter), desc=repr(model)) if verbose else splitter
    )
    df = pd.concat(
        [
            model.fit(train, past_covariates=past_covariates).predict(test.index)
            for train, test, past_covariates in _splits
        ]
    )
    df.index.name = splitter.index.name
    if isinstance(splitter.index, pd.MultiIndex):
        df.columns = pd.MultiIndex.from_tuples(df.columns)
    df.columns.set_names(splitter.columns.names, inplace=True)
    return add_prediction_number(df, splitter.num_predict)


@overload
def add_prediction_number(df: pd.DataFrame, n: int, inplace: Literal[True]) -> None:
    ...


@overload
def add_prediction_number(
    df: pd.DataFrame, n: int, inplace: Literal[False] = False
) -> pd.DataFrame:
    ...


def add_prediction_number(
    df: pd.DataFrame, n: int, inplace: bool = False
) -> Union[None, pd.DataFrame]:
    """Add ``Prediction #`` level to multi-step prediction dataframe.

    Parameters
    ----------
    df : pd.DataFrame
        Original dataframe which was generating by making ``n`` predictions into future
        for each time step.
    n : int
        Number of predictions per time step.
    inplace : bool
        If ``True``, return ``None`` and mutate input dataframe. Return new dataframe
        otherwise.

    Returns
    -------
    pd.DataFrame
    """
    assert len(df) % n == 0
    if not inplace:
        df = df.copy()
    df[INDEX_LEVEL1_NAME] = np.tile(  # type: ignore [no-untyped-call]
        range(n),
        len(df) // n,
    )
    return df.set_index(
        INDEX_LEVEL1_NAME, append=True, inplace=inplace
    )  # type: ignore [call-overload]
