"""Functions for reading/writing data to file."""
from os import PathLike
from pathlib import Path
from typing import Optional, Union

import pandas as pd

DATAPATH = Path(__file__).parent / "public.csv.gz"


def read_csv(
    filepath: Optional[Union[str, PathLike]] = None, is_prediction: bool = False
) -> pd.DataFrame:
    """Read GDELT data.

    Input files must have the following properties:

    * The column index must a two-level MultiIndex with names "Region" and "Event"
    * The index is a flat :class:`pandas.PeriodIndex` with frequency ``"W"``.
    * Optionally, the second column may be secondary index (usually named
      ``Prediction #``).

    Parameters
    ----------
    filepath:  Optional[Union[str, PathLike]]
        Path to input file. Points to ATD2022 public dataset by default.
    is_prediction: bool
        If True, assume the data has a ``Prediction #`` column in the second column.
        Otherwise, assume the data only has a flat index consisting of the ``ISODATE``.
        ``Prediction #`` indexes (default = ``False``).

    Returns
    -------
    pd.DataFrame
    """
    filepath = filepath if filepath is not None else DATAPATH
    index_col = [0, 1] if is_prediction else [0]
    df = pd.read_csv(filepath, index_col=index_col, header=[0, 1])
    time_index = pd.PeriodIndex(df.index.get_level_values(0), freq="W")
    return (
        df.set_axis(
            pd.MultiIndex.from_arrays((time_index, df.index.get_level_values(1)))
        )
        if is_prediction
        else df.set_axis(time_index)
    )
