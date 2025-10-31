import pandas as pd
from hypothesis import given, settings, strategies as st

from atd2022.backtest import Splitter

from .strategies import multivariate_timeseries


@given(
    df=multivariate_timeseries(max_columns=1, min_rows=23, max_rows=25),
    window=st.integers(min_value=1, max_value=10),
    num_predict=st.integers(min_value=1, max_value=3),
    gap=st.integers(min_value=0, max_value=10),
    slide=st.integers(min_value=1, max_value=10),
    expanding=st.booleans(),
)
@settings(max_examples=50)
def test_len_splitter_matches_len_iterable(
    df: pd.DataFrame,
    window: int,
    num_predict: int,
    gap: int,
    slide: int,
    expanding: bool,
) -> None:
    s = Splitter(
        df,
        window=window,
        num_predict=num_predict,
        gap=gap,
        slide=slide,
        expanding=expanding,
    )
    assert len(s) == len(list(s))


@given(
    df=multivariate_timeseries(max_columns=1, min_rows=23, max_rows=25),
    window=st.integers(min_value=1, max_value=10),
    num_predict=st.integers(min_value=1, max_value=3),
    gap=st.integers(min_value=0, max_value=10),
    slide=st.integers(min_value=1, max_value=10),
    expanding=st.booleans(),
)
@settings(max_examples=50)
def test_len_train_geq_window(
    df: pd.DataFrame,
    window: int,
    num_predict: int,
    gap: int,
    slide: int,
    expanding: bool,
) -> None:
    s = Splitter(
        df,
        window=window,
        num_predict=num_predict,
        gap=gap,
        slide=slide,
        expanding=expanding,
    )
    for train, _, _ in s:
        if expanding:
            assert len(train) >= window
        else:
            assert len(train) == window


@given(
    df=multivariate_timeseries(max_columns=1, min_rows=23, max_rows=25),
    window=st.integers(min_value=1, max_value=10),
    num_predict=st.integers(min_value=1, max_value=3),
    gap=st.integers(min_value=0, max_value=10),
    slide=st.integers(min_value=1, max_value=10),
    expanding=st.booleans(),
)
@settings(max_examples=50)
def test_len_test_matches_num_predict(
    df: pd.DataFrame,
    window: int,
    num_predict: int,
    gap: int,
    slide: int,
    expanding: bool,
) -> None:
    s = Splitter(
        df,
        window=window,
        num_predict=num_predict,
        gap=gap,
        slide=slide,
        expanding=expanding,
    )
    for _, test, _ in s:
        assert len(test) == num_predict


@given(
    df=multivariate_timeseries(max_columns=1, min_rows=23, max_rows=25),
    window=st.integers(min_value=1, max_value=10),
    num_predict=st.integers(min_value=1, max_value=3),
    gap=st.integers(min_value=0, max_value=10),
    slide=st.integers(min_value=1, max_value=10),
    expanding=st.booleans(),
)
@settings(max_examples=10)
def test_past_covariates_is_none_given_no_data(
    df: pd.DataFrame,
    window: int,
    num_predict: int,
    gap: int,
    slide: int,
    expanding: bool,
) -> None:
    s = Splitter(
        df,
        window=window,
        num_predict=num_predict,
        gap=gap,
        slide=slide,
        expanding=expanding,
    )
    for _, _, past_covariates in s:
        assert past_covariates is None


@given(
    df=multivariate_timeseries(max_columns=1, min_rows=25, max_rows=30),
    window=st.integers(min_value=1, max_value=10),
    num_predict=st.integers(min_value=1, max_value=5),
    gap=st.integers(min_value=0, max_value=10),
    slide=st.integers(min_value=1, max_value=10),
    expanding=st.booleans(),
)
@settings(max_examples=10)
def test_past_covariates_index_matches_train_index(
    df: pd.DataFrame,
    window: int,
    num_predict: int,
    gap: int,
    slide: int,
    expanding: bool,
) -> None:
    s = Splitter(
        df,
        window=window,
        num_predict=num_predict,
        gap=gap,
        slide=slide,
        past_covariates=df,
        expanding=expanding,
    )
    for train, _, past_covariates in s:
        pd.testing.assert_index_equal(train.index, past_covariates.index)
