"""Tools for evaluating metrics in batches."""
from typing import Callable, Collection, Optional

import pandas as pd
from tqdm.auto import tqdm

from ..backtest import INDEX_LEVEL1_NAME
from .types import Metric

__all__ = [
    "NotMetricError",
    "InvalidMetricModeError",
    "compute_metrics",
    "compute_nstep_metrics",
]


class NotMetricError(Exception):
    """Metric does not support :class:`~atd2022.metrics.types.Metric` protocol."""


class InvalidMetricModeError(Exception):
    """Attempted to use metric in incorrect mode."""


def _get_name(f: Callable) -> str:
    return f.__name__ if hasattr(f, "__name__") else f.__class__.__name__


def compute_metrics(
    true: pd.DataFrame,
    preds: Collection[pd.DataFrame],
    metric_fcns: Collection[Metric],
    models: Optional[Collection[object]] = None,
    train: Optional[pd.DataFrame] = None,
    columnwise: bool = False,
    verbose: bool = False,
) -> pd.DataFrame:
    """Generate a table summarizing metric results for all predictions.

    Parameters
    ----------
    true: pd.DataFrame
        Truth.
    preds: Collection[pd.DataFrame]
        List of predictions.
    metric_fcns: Collection[Metric]
        Metric functions supporting the Metric protocol.
    models: Optional[Collection[object]]
        Used to genenerate a name for the model (via its str). If ``None``, the index
        of the resulting metrics dataframe will be a :class:`pandas.RangeIndex`.
    train: Optional[pd.DataFrame]
        Training data used. Only necessary for metrics that rely on the training data
        for scaling (default=``None``).
    columnwise: bool
        If ``True``, the columns of the metrics dataframe will be a
        :class:`pandas.MultiIndex` with the first k levels corresponding to the k
        levels of the input dataframes (e.g., ``true.columns.name``) and the final
        level corresponding to the metric name. If ``columnwise`` is ``True``, all
        ``metric_fcns`` must have ``multioutput='raw_values'``.
    verbose: bool
        If ``True``, show progress bar (default= ``False``).

    Returns
    -------
    pd.DataFrame
        If ``columnwise`` is ``False``, the resulting dataframe will be of shape
        ``(n_models, n_metrics)``. Otherwise, the dataframe will be of shape
        ``(n_models, n_metrics*true.shape[1])``.
    """
    if any(not isinstance(f, Metric) for f in metric_fcns):
        raise NotMetricError("All metrics must adhere to the atd2022 Metric protocol.")

    if columnwise:
        if any(f.multioutput != "raw_values" for f in metric_fcns):
            raise InvalidMetricModeError(
                "All metrics must have multioutput='raw_values' if columnwise=True."
            )
    else:
        if any(f.multioutput == "raw_values" for f in metric_fcns):
            raise InvalidMetricModeError(
                "All metrics must not have multioutput='raw_values'"
                " if columnwise=False."
            )

    if models is None:
        models = pd.Series(range(len(preds)))
    apply_metrics = (
        _compute_metrics_columnwise if columnwise else _compute_metrics_uniform
    )
    return apply_metrics(true, preds, metric_fcns, models, train=train, verbose=verbose)


def _compute_metrics_uniform(
    true: pd.DataFrame,
    preds: Collection[pd.DataFrame],
    metric_fcns: Collection[Metric],
    models: Collection[object],
    train: Optional[pd.DataFrame] = None,
    verbose: bool = False,
) -> pd.DataFrame:
    pbar = _to_pbar(models, preds) if verbose else zip(models, preds)
    return pd.DataFrame.from_dict(
        {
            str(model): {
                _get_name(metric): metric(
                    y_true=true.loc[pred.index.get_level_values(0)],
                    y_pred=pred,
                    y_train=train,
                )
                for metric in metric_fcns
            }
            for model, pred in pbar
        },
        orient="index",
    ).sort_index(axis=1)


def _compute_metrics_columnwise(
    true: pd.DataFrame,
    preds: Collection[pd.DataFrame],
    metric_fcns: Collection[Metric],
    models: Collection[object],
    train: Optional[pd.DataFrame] = None,
    verbose: bool = False,
) -> pd.DataFrame:
    metrics: dict = {}
    pbar = _to_pbar(models, preds) if verbose else zip(models, preds)
    for model, pred in pbar:
        metrics[str(model)] = {}
        for metric in metric_fcns:
            _metrics = metric(
                y_true=true.loc[pred.index.get_level_values(0)],
                y_pred=pred,
                y_train=train,
            )
            assert isinstance(_metrics, Collection)
            for col, m in zip(true.columns, _metrics):
                _col = (col,) if not isinstance(col, tuple) else col
                metrics[str(model)][(*_col, _get_name(metric))] = m
    return pd.DataFrame.from_dict(metrics, orient="index").sort_index(axis=1)


def _to_pbar(models: Collection[object], preds: Collection[pd.DataFrame]) -> tqdm:
    return tqdm(
        zip(models, preds),
        desc="Predictions Processed",
        total=min(len(models), len(preds)),
    )


def compute_nstep_metrics(
    true: pd.DataFrame,
    preds: Collection[pd.DataFrame],
    metric_fcns: Collection[Metric],
    models: Collection[object],
    train: Optional[pd.DataFrame] = None,
    verbose: bool = False,
) -> pd.DataFrame:
    """Split data by num_predict and compute metrics on each split.

    Parameters
    ----------
    true: pd.DataFrame
        Truth.
    preds: Collection[pd.DataFrame]
        List of predictions.
    metric_fcns: Collection[Metric]
        Metric functions supporting the Metric protocol.
    models: Optional[Collection[object]]
        Used to genenerate a name for the model (via its ``repr``). If ``None``, the
        index of the resulting metrics dataframe will be a :class:`pandas.RangeIndex`.
    train: Optional[pd.DataFrame]
        Training data used. Only necessary for metrics that rely on the training data
        for scaling (default= ``None``).
    verbose: bool
        If ``True``, show progress bar (default= ``False``).

    Returns
    -------
    pd.DataFrame
        Dataframe with class:`pandas.MultiIndex` with levels ``["model", "n"]`` and
        columns equal to the ``repr`` s for ``metric_fcns``.
    """
    if not preds:
        raise ValueError("Must provide at least one prediction.")

    for pred in preds:
        _index_is_valid(pred)

    # Ensure all predictions have the same "Prediction #" index
    all_n_labels = [sorted(pred.index.get_level_values(1).unique()) for pred in preds]
    n_labels = all_n_labels[0]
    for labels in all_n_labels:
        if n_labels != labels:
            raise ValueError("Level 1 index values must match for all predictions.")

    preds_grouped_by_n = [pred.groupby(INDEX_LEVEL1_NAME) for pred in preds]
    preds_n = [
        model_groups.get_group(n)
        for model_groups in preds_grouped_by_n
        for n in sorted(model_groups.groups)
    ]
    metrics_n = compute_metrics(
        true, preds_n, metric_fcns, train=train, verbose=verbose
    )
    model_n_index = pd.MultiIndex.from_product(
        ([str(model) for model in models], n_labels),
        names=["model", "n"],
    )
    return metrics_n.set_axis(model_n_index).sort_index()


def _index_is_valid(df: pd.DataFrame) -> None:
    if not isinstance(df.index, pd.MultiIndex):
        raise ValueError("Index is not a MultiIndex.")
    if df.index.names[1] != INDEX_LEVEL1_NAME:
        raise ValueError(f"Index level1 name is not {INDEX_LEVEL1_NAME}.")
