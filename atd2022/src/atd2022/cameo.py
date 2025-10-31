"""Make CAMEO/FIPS codes human readable."""
from functools import cache
from pathlib import Path
from typing import List, Union

import numpy as np
import pandas as pd
from numpy.typing import ArrayLike

__all__ = ["Cameo", "_Cameo"]


class _Cameo:
    """Cameo code translator.

    Note
    ----
    Users should generally use the pre-instantiated :const:`atd2022.cameo.Cameo` rather
    than interacting with :class:`atd2022.cameo._Cameo` directly.
    """

    @staticmethod
    @cache
    def _read_csv(filename: str) -> pd.Series:
        # Do not set index_col in read_csv to ensure we read all columns as str
        df = pd.read_csv(
            Path(__file__).resolve().parent / f"cameo_data/{filename}",
            dtype=str,
        )
        return df.set_index(df.columns[0]).iloc[:, 0]

    @property
    def events(self) -> pd.Series:
        """A dataframe containing cameo codes and their descriptions."""
        return self._read_csv("eventcodes.csv")

    @property
    def countries(self) -> pd.Series:
        """A dataframe containing FIPS country codes and their full names."""
        return self._read_csv("fips.csv")

    def countries_from_codes(self, country_codes: ArrayLike) -> List[str]:
        """Convert FIPS country codes to their full names.

        Parameters
        ----------
        country_codes : np.typing.ArrayLike
            An array-like of country codes from the index of
            :attr:`atd2022.cameo._Cameo.countries`.

        Returns
        -------
        List[str]
            Country full names.

        Examples
        --------
        >>> Cameo.countries_from_codes(["US", "CA"])
        ['United States', 'Canada']
        """
        return _as_list(self.countries.loc[np.asarray(country_codes)])

    def countries_to_codes(self, country_strs: ArrayLike) -> List[str]:
        """Convert FIPS country codes to their full names.

        Parameters
        ----------
        country_strs : np.typing.ArrayLike
            An array-like of country full names from the values of
            :attr:`atd2022.cameo._Cameo.countries`.

        Returns
        -------
        List[str]
            Country FIPS codes.

        Examples
        --------
        >>> Cameo.countries_to_codes(["United States", "Canada"])
        ['US', 'CA']
        """
        return _as_list(_swap_index(self.countries).loc[np.asarray(country_strs)])

    def events_from_codes(self, event_codes: ArrayLike) -> List[str]:
        """Convert CAMEO event codes to their full descriptions.

        Parameters
        ----------
        event_codes : np.typing.ArrayLike
            An array-like of event codes from the index of
            :meth:`atd2022.cameo._Cameo.events`.

        Returns
        -------
        List[str]
            Event descriptions.

        Examples
        --------
        >>> Cameo.events_from_codes(["01", "20"])
        ['MAKE PUBLIC STATEMENT', 'USE UNCONVENTIONAL MASS VIOLENCE']
        """
        return _as_list(self.events.loc[np.asarray(event_codes)])

    def events_to_codes(self, event_codes: ArrayLike) -> List[str]:
        """Convert CAMEO event descriptions to their event code.

        Parameters
        ----------
        event_codes : np.typing.ArrayLike
            An array-like of event descriptions from the values of
            :meth:`atd2022.cameo._Cameo.events`.

        Returns
        -------
        List[str]
            Event codes.

        Examples
        --------
        >>> Cameo.events_to_codes(
        ...     ["MAKE PUBLIC STATEMENT", "USE UNCONVENTIONAL MASS VIOLENCE"]
        ... )
        ['01', '20']
        """
        return _as_list(_swap_index(self.events).loc[np.asarray(event_codes)])


def _swap_index(x: pd.Series) -> pd.Series:
    return x.reset_index().set_index(x.name).iloc[:, 0]


def _as_list(x: Union[str, pd.Series]) -> List[str]:
    return x.to_list() if isinstance(x, pd.Series) else [x]


# Create instance, but don't delete so we can have Sphinx docs.
Cameo = _Cameo()
"""Cameo code translator.

Note
----
Users should prefer to use this pre-instantiated object rather than creating their own
:class:`atd2022.cameo._Cameo` instance.
"""
