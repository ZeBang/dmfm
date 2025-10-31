"""Interactive `holoview` panels for comparing multiple time series."""
from typing import Any, Callable, Dict, List

import holoviews as hv
import pandas as pd
import panel as pn

from ..cameo import Cameo

__all__ = [
    "plot_series_panel",
    "plot_series_nstep_panel",
    "NotMultiIndexError",
    "IncorrectlyNamedIndexError",
    "MismatchedColumnsError",
]

DataFrameMapping = Dict[str, pd.DataFrame]
INDEX_LEVEL1_NAME = "Prediction #"
COL_LEVEL_NAMES = ("Region", "Event")
HV_DEFAULT_OPTS = {
    "height": 400,
    "width": 650,
    "xlabel": "Date",
    "ylabel": "Event Count",
    "framewise": True,
}


class NotMultiIndexError(Exception):
    pass


class IncorrectlyNamedIndexError(Exception):
    pass


class MismatchedColumnsError(Exception):
    pass


def plot_series_panel(
    timeseries: DataFrameMapping,
) -> pn.Row:
    """Generate a holoviews panel for comparing timeseries.

    To set backend (`matplotlib`, `bokeh`, `ploty`), use `hv.extension(backend)`
    in the caller.

    Parameters
    ----------
    timeseries: Dict[str, pd.DataFrame]
        Dictionary mapping label string to timeseries.

    Returns
    -------
    pn.Row
    """
    # Ensure provided timeseries are valid
    if len(timeseries) == 0:
        raise IndexError("Must provide nonzero number of timeseries.")

    # Get arbitrary timeseries
    example_df = _get_examplar(timeseries)

    # Ensure all timeseries columns match the example df
    _check_each_columns_match_examplar(timeseries, example_df)

    # Ensure all columns are pd.MultiIndex with appropriate column names.
    _check_columns(timeseries)

    def _load_data(region: Any, event: Any, **kwargs: Any) -> hv.Overlay:
        # Uses `timeseries` from parent namespace
        region_code = Cameo.countries_to_codes(region)[0]
        event_code = Cameo.events_to_codes(event)[0]
        return hv.Overlay(
            _get_curves(timeseries, _get_data, event_code, region_code)
        ).opts(
            title=f"Region: {region}, Event: {event}",
            **HV_DEFAULT_OPTS,
        )

    # Create dynamic map with region and event drop-down menus
    country_codes = sorted(list(set(example_df.columns.get_level_values(0))))
    event_codes = sorted(list(set(example_df.columns.get_level_values(1))))
    return pn.panel(
        hv.DynamicMap(_load_data, kdims=["Region", "Event"])
        .redim.values(
            Region=Cameo.countries_from_codes(country_codes),
            Event=Cameo.events_from_codes(event_codes),
        )
        .opts(framewise=True),
        center=True,
        widget_location="right",
    )


def plot_series_nstep_panel(
    timeseries: Dict[str, pd.DataFrame],
    timeseries_nstep: Dict[str, pd.DataFrame],
) -> pn.Row:
    """Generate a holoviews panel for comparing timeseries.

    To set backend (`matplotlib`, `bokeh`, `ploty`), use `hv.extension(backend)`
    in the caller.

    Parameters
    ----------
    timeseries: Dict[str, pd.DataFrame]
        Dictionary mapping label string to timeseries.
    timeseries_nstep: Dict[str, pd.DataFrame]
        Dictionary mapping label string to timeseries that has n predictions for each
        time step.

    Returns
    -------
    pn.Row
    """
    # If user didn't provide any nstep series, delegate to plot_series_panel.
    if not timeseries_nstep:
        return plot_series_panel(timeseries)

    # Get arbitrary timeseries_nstep
    example_df = _get_examplar(timeseries_nstep)

    # Ensure all columns match the example df
    _check_each_columns_match_examplar(timeseries, example_df)
    _check_each_columns_match_examplar(timeseries_nstep, example_df)

    # Ensure all columns are pd.MultiIndex with appropriate column names.
    _check_columns(timeseries)

    # Ensure all index in timeseries_nstep are pd.MultiIndex and level1 name is
    # INDEX_LEVEL1_NAME
    _check_index(timeseries_nstep)

    def _load_data(region: Any, event: Any, step: int, **kwargs: Any) -> hv.Overlay:
        # Uses `timeseries` and `timeseries_nstep` from parent namespace
        region_code = Cameo.countries_to_codes(region)[0]
        event_code = Cameo.events_to_codes(event)[0]
        curves = _get_curves(timeseries, _get_data, event_code, region_code)
        curves_nstep = _get_curves(
            timeseries_nstep, _get_data_nstep, event_code, region_code, step
        )
        return hv.Overlay(curves + curves_nstep).opts(
            title=f"Region: {region}, Event: {event}, Step: {step}",
            **HV_DEFAULT_OPTS,
        )

    # Create dynamic map with region and event drop-down menus
    country_codes = sorted(list(set(example_df.columns.get_level_values(0))))
    event_codes = sorted(list(set(example_df.columns.get_level_values(1))))
    return pn.panel(
        hv.DynamicMap(_load_data, kdims=["Region", "Event", "Step"])
        .redim.values(
            Region=Cameo.countries_from_codes(country_codes),
            Event=Cameo.events_from_codes(event_codes),
            Step=sorted(list(set(example_df.index.get_level_values(1)))),
        )
        .opts(framewise=True),
        center=True,
        widget_location="right",
    )


# Getters
def _get_curves(
    dfs: DataFrameMapping, func: Callable, col: Any, *func_args: Any
) -> List[hv.Curve]:
    return [
        hv.Curve(func(df, *func_args), ("date", "Date"), col, label=label).opts(
            framewise=True,
        )
        for label, df in dfs.items()
    ]


def _get_data(data: pd.DataFrame, region: Any) -> pd.DataFrame:
    index_name = "index" if data.index.name is None else data.index.name
    df = getattr(data, region).reset_index().rename(columns={index_name: "date"})
    df["date"] = df["date"].dt.to_timestamp()
    return df


def _get_data_nstep(data: pd.DataFrame, region: Any, step: int) -> pd.DataFrame:
    # Access the rows corresponding to current step.
    data = data.swaplevel().loc[step]
    # Do everything else
    return _get_data(data, region)


def _get_examplar(d: Dict[str, pd.DataFrame]) -> pd.DataFrame:
    # Get arbitrary timeseries
    label, example = d.popitem()
    d[label] = example
    return example


# Checkers
def _check_columns(dfs: DataFrameMapping) -> None:
    for name, df in dfs.items():
        if not isinstance(df.columns, pd.MultiIndex):
            raise NotMultiIndexError(f"Dataframe ({name}) must be a pd.MultiIndex.")
        if tuple(df.columns.names) != COL_LEVEL_NAMES:
            raise IncorrectlyNamedIndexError(
                f"Dataframe ({name}) must have column level names equal to "
                f"{COL_LEVEL_NAMES}."
            )


def _check_each_columns_match_examplar(
    dfs: Dict[str, pd.DataFrame], example: pd.DataFrame
) -> None:
    for series in dfs.values():
        if not series.columns.equals(example.columns):
            raise MismatchedColumnsError("All timeseries columns-indices must match.")


def _check_index(dfs: DataFrameMapping) -> None:
    for name, df in dfs.items():
        if not isinstance(df.index, pd.MultiIndex):
            raise NotMultiIndexError(f"Dataframe ({name}) must be a pd.MultiIndex.")
        if len(df.index.names) != 2 or df.index.names[1] != INDEX_LEVEL1_NAME:
            raise IncorrectlyNamedIndexError(
                f"Dataframe ({name}) must have index level names of length two with"
                f" the level-1 named {INDEX_LEVEL1_NAME}."
            )
