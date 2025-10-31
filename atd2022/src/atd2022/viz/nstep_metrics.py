"""Plot nstep metrics."""
import pandas as pd
import seaborn as sns
from matplotlib.ticker import MaxNLocator

__all__ = ["plot_nstep_metrics"]


def plot_nstep_metrics(metrics_n: pd.DataFrame) -> sns.FacetGrid:
    """Plot metric values vs. prediction number for all metrics.

    Parameters
    ----------
    metrics_n: pd.DataFrame
        Generated from `atd2022.metrics.compute_nstep_metrics`.

    Returns
    -------
    sns.FacetGrid
    """
    melted_df = metrics_n.reset_index().melt(["model", "n"], var_name="metric")
    fg = (
        sns.FacetGrid(
            data=melted_df,
            col="metric",
            height=5,
            sharey=False,
            col_wrap=3,
            legend_out=True,
        )
        .map_dataframe(sns.lineplot, x="n", y="value", hue="model")
        .add_legend()
    )
    for ax in fg.axes.flat:
        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
    return fg
