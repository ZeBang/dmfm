"""Quadrant plot comparing timeseries behavior."""
import itertools
from dataclasses import dataclass
from typing import List, Tuple

import numpy as np
import pandas as pd
import seaborn as sns
from matplotlib import pyplot as plt
from matplotlib.patches import Rectangle

from ..metrics import classify_behavior

__all__ = ["plot_behavior"]


@dataclass
class _Span:
    a: float
    b: float

    def diff(self) -> float:
        return self.b - self.a


def plot_behavior(
    df: pd.DataFrame, adi_thresh: float = 1.32, cov2_thresh: float = 0.49
) -> plt.Axes:
    """Plot time series in quadrant plot categorizing time series behavior.

    Parameters
    ----------
    df : pd.DataFrame
    adi_thresh : float
    cov2_thresh : float

    Returns
    -------
    plt.Axes

    See Also
    --------
    atd2022.metrics.intermittent.eda.classify_behavior
    atd2022.metrics.intermittent.eda.adi
    atd2022.metrics.intermittent.eda.cov
    """

    behavior = classify_behavior(df, detailed=True)
    behavior["adi"], min_adi = _clamp_to_positive(behavior["adi"])
    behavior["cov2"], min_cov2 = _clamp_to_positive(behavior["cov2"])
    ax = sns.scatterplot(data=behavior, x="cov2", y="adi", style="behavior")
    ax.set(xscale="log", yscale="log")
    ax.axhline(adi_thresh, color="black", linestyle="--")
    ax.axvline(cov2_thresh, color="black", linestyle="--")
    ax.set_title("Timeseries Behavior")

    _ensure_all_quadrants_shown(
        ax,
        default_x_max=2 * cov2_thresh,
        default_y_max=2 * adi_thresh,
        x_min=min_cov2,
        y_min=min_adi,
    )
    ax.autoscale(False)

    for patch in _make_quadrant_patches(ax, adi_thresh, cov2_thresh):
        ax.add_patch(patch)

    ax.legend(loc="center left", bbox_to_anchor=(1.0, 0.5))
    return ax


def _clamp_to_positive(x: pd.Series) -> Tuple[pd.Series, float]:
    """Set small values to some small, non-zero number.

    Try to set as the same, but negative exponent as the largest value. If largest
    exponent is not strictly positive, set to exp(-3).

    Parameters
    ----------
    x : pd.Series

    Returns
    -------
    pd.Series
        Clamped series.
    float
        Clamp value.
    """
    x = x.copy()[np.isfinite(x)]
    exponent = np.floor(np.log(x).max())
    exponent = exponent if exponent > 0 else 3
    min_x = np.exp(-exponent)
    x[x < min_x] = min_x
    return x, min_x


def _ensure_all_quadrants_shown(
    ax: plt.Axes,
    *,
    default_x_max: float,
    default_y_max: float,
    x_min: float,
    y_min: float,
) -> None:
    """Increase the axes limits to span all four quadrants."""
    _, x_max = ax.get_xlim()
    _, y_max = ax.get_ylim()

    x_max = max(x_max, default_x_max)
    y_max = max(y_max, default_y_max)

    ax.set_xlim(x_min, x_max)
    ax.set_ylim(y_min, y_max)


def _make_quadrant_patches(
    ax: plt.Axes, adi_thresh: float, cov2_thresh: float
) -> List[Rectangle]:
    """Generate color patches for each quadrant.

    Parameters
    ----------
    ax : plt.Axes
    adi_thresh : float
    cov2_thresh : float

    Returns
    -------
    List[Rectangle]
    """
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()

    spans = {
        "left": _Span(xmin, cov2_thresh),
        "right": _Span(cov2_thresh, xmax),
        "bottom": _Span(ymin, adi_thresh),
        "top": _Span(adi_thresh, ymax),
    }
    quadrants = itertools.product(("left", "right"), ("bottom", "top"))

    colors = ["blue", "yellow", "green", "red"]
    labels = ["Smooth", "Intermittent", "Erratic", "Lumpy"]
    return [
        Rectangle(
            (spans[x_side].a, spans[y_side].a),
            spans[x_side].diff(),
            spans[y_side].diff(),
            facecolor=color,
            alpha=0.2,
            label=label,
        )
        for (x_side, y_side), color, label in zip(quadrants, colors, labels)
    ]
