from typing import Tuple

import pandas as pd
import panel as pn
import pytest
from hypothesis import given, settings

import atd2022.viz as atdv

from .strategies import matching_multivariate_timeseries, multivariate_timeseries

# Tests for regular plot_series_panel


@given(
    true_pred=matching_multivariate_timeseries(
        max_columns=3,
        min_rows=2,
        max_rows=20,
        column_type="multi",
    )
)
@settings(max_examples=1, deadline=None)
def test_panel_matching_pair_valid(
    true_pred: Tuple[pd.DataFrame, pd.DataFrame]
) -> None:
    true, pred = true_pred
    true.columns.names = ["Region", "Event"]
    pred.columns.names = ["Region", "Event"]

    # Since these dfs match (by definition) this should complete successfully
    timeseries = {"true": true, "pred": pred}
    fig = atdv.plot_series_panel(timeseries)
    assert isinstance(fig, pn.Row)


def test_panel_empty_timeseries_mapping() -> None:
    with pytest.raises(IndexError):
        atdv.plot_series_panel(dict())


@given(
    true_pred=matching_multivariate_timeseries(
        min_columns=3,
        max_columns=3,
        min_rows=2,
        max_rows=4,
        column_type="flat",
    )
)
@settings(max_examples=1, deadline=None)
def test_panel_mismatched_columns(true_pred: Tuple[pd.DataFrame, pd.DataFrame]) -> None:
    true, pred = true_pred
    # Make columns not match
    true.columns = ["a", "b", "c"]
    pred.columns = ["d", "e", "f"]
    timeseries = {"true": true, "pred": pred}
    with pytest.raises(atdv.MismatchedColumnsError):
        atdv.plot_series_panel(timeseries)


@given(
    df=multivariate_timeseries(
        min_columns=3,
        max_columns=3,
        min_rows=2,
        max_rows=4,
        column_type="flat",
    )
)
@settings(max_examples=1, deadline=None)
def test_not_multiindex(df: pd.DataFrame) -> None:
    with pytest.raises(atdv.NotMultiIndexError):
        atdv.plot_series_panel({"df": df})


@given(
    df=multivariate_timeseries(
        min_columns=3,
        max_columns=3,
        min_rows=2,
        max_rows=4,
        column_type="multi",
    )
)
@settings(max_examples=1, deadline=None)
def test_columns_not_named_region_event(df: pd.DataFrame) -> None:
    df.columns.names = ["not region", "not event"]
    with pytest.raises(atdv.IncorrectlyNamedIndexError):
        atdv.plot_series_panel({"df": df})


# Tests for plot_series_nstep_panel


@given(
    df=multivariate_timeseries(
        min_columns=3,
        max_columns=3,
        min_rows=2,
        max_rows=4,
        column_type="multi",
    )
)
@settings(max_examples=1, deadline=None)
def test_nstep_valid(df: pd.DataFrame) -> None:
    df.columns.names = ["Region", "Event"]
    new_index = pd.MultiIndex.from_product(
        (df.index, range(2)),
        names=(df.index.name, "Prediction #"),
    )
    df_nstep = pd.concat([df, df]).set_axis(new_index)
    p = atdv.plot_series_nstep_panel({"df": df}, {"nstep": df_nstep})

    assert isinstance(p, pn.Row)
    # Make sure step parameter is included
    assert len(p.objects[3].objects[1]) == 3


@given(
    df=multivariate_timeseries(
        min_columns=3,
        max_columns=3,
        min_rows=2,
        max_rows=4,
        column_type="multi",
    )
)
@settings(max_examples=1, deadline=None)
def test_nstep_delegates_to_simple_panel_when_no_nstep_series_provided(
    df: pd.DataFrame,
) -> None:
    df.columns.names = ["Region", "Event"]
    p = atdv.plot_series_nstep_panel({"df": df}, dict())

    assert isinstance(p, pn.Row)
    # Check that step parameter is not included
    assert len(p.objects[3].objects[1]) == 2


@given(
    df=multivariate_timeseries(
        min_columns=3,
        max_columns=3,
        min_rows=2,
        max_rows=4,
        column_type="multi",
    )
)
@settings(max_examples=1, deadline=None)
def test_nstep_index_not_multiindex(df: pd.DataFrame) -> None:
    df.columns.names = ["Region", "Event"]
    # Index is not a MultiIndex
    with pytest.raises(atdv.NotMultiIndexError):
        atdv.plot_series_nstep_panel(dict(), {"df": df})


@given(
    df=multivariate_timeseries(
        min_columns=3,
        max_columns=3,
        min_rows=2,
        max_rows=4,
        column_type="multi",
    )
)
@settings(max_examples=1, deadline=None)
def test_nstep_index_level1_name_incorrect(df: pd.DataFrame) -> None:
    df.columns.names = ["Region", "Event"]
    new_index = pd.MultiIndex.from_product(
        (df.index, range(2)),
        names=(df.index.name, "Not Prediction #"),
    )
    df = pd.concat([df, df]).set_axis(new_index)
    # Index has incorrect Level1 name
    with pytest.raises(atdv.IncorrectlyNamedIndexError):
        atdv.plot_series_nstep_panel(dict(), {"df": df})
