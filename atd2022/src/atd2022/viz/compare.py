"""A static `matplotlib`-based predictions vs. ground truth plotter."""
from typing import Collection, Iterable, Optional, Union

import pandas as pd
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator

__all__ = ["plot_pred_vs_true"]


def plot_pred_vs_true(
    true: pd.DataFrame,
    preds: Union[pd.DataFrame, Collection[pd.DataFrame]],
    models: Optional[Iterable[object]] = None,
) -> plt.Figure:
    """Generate a subplot of truth vs. all predictions for each time series in truth.

    This function uses the matplotlib backend to generate static figures.

    Parameters
    ----------
    true: pd.DataFrame
    preds: Union[pd.DataFrame, Collection[pd.DataFrame]]
    models: Optional[Iterable[object]]
        Objects whose repr will be used in Legend to show which model generated
        the corresponding forecast. If None, a RangeIndex is generated and used.

    Returns
    -------
    plt.Figure
        A plot with (true.shape[1], 1) subplots (one for each time series in true),
        each comparing truth vs. predicted.
    """
    if isinstance(preds, pd.DataFrame):
        preds = [preds]

    if models is None:
        models = pd.Series(range(len(preds)))

    fig, axs = plt.subplots(true.shape[1], sharex=True, figsize=(15, 3 * true.shape[1]))

    if isinstance(axs, plt.Axes):
        axs = [axs]

    for ax, (column, true_series) in zip(axs, true.iteritems()):
        true_series.plot(ax=ax, label="Truth", title=repr(column), ylabel="Count")
        for model, pred in zip(models, preds):
            pred[column].plot(ax=ax, label=repr(model))

    for ax in axs:
        ax.yaxis.set_major_locator(MaxNLocator(integer=True))
        ax.legend(loc="center left", bbox_to_anchor=(1.0, 0.5))

    fig.tight_layout()
    return fig
