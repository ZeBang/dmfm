from pathlib import Path
from typing import Any, Dict, List, Sequence

import pandas as pd
import panel as pn
from matplotlib.cm import get_cmap
from matplotlib.colors import to_hex
from sktime.performance_metrics.forecasting import (
    MeanAbsoluteScaledError,
    MeanSquaredError,
)

import atd2022
from atd2022.cameo import Cameo

try:
    import plotly.express as px
except ModuleNotFoundError:
    raise ModuleNotFoundError(
        "Cannot import plotly. Be sure to install the `leaderboard` extra."
    )

pn.extension()


def read_all_predictions(results_dir: Path) -> Dict[str, pd.DataFrame]:
    return {
        p.stem.split(".")[0]: atd2022.io.read_csv(p, is_prediction=True)
        for p in results_dir.glob("*.csv*")
    }


def make_avg_metrics_table(
    true: pd.DataFrame, preds: Dict[str, pd.DataFrame]
) -> pd.DataFrame:
    metric_functions = [
        MeanAbsoluteScaledError(),
        MeanSquaredError(square_root=True),
        atd2022.metrics.Spec(),
    ]

    metrics = atd2022.metrics.compute_metrics(
        true,
        preds.values(),
        metric_functions,
        models=preds.keys(),
        train=true,
        verbose=True,
    ).sort_values(by="MeanAbsoluteScaledError")

    metrics = metrics.rename(
        columns={
            "MeanAbsoluteScaledError": "MASE",
            "MeanSquaredError": "RMSE",
            "Spec": "SPEC",
        }
    )
    metrics.index.name = "Team"
    return metrics


def make_raw_metrics_table(
    true: pd.DataFrame, preds: Dict[str, pd.DataFrame]
) -> pd.DataFrame:
    metric_functions = [
        MeanAbsoluteScaledError(multioutput="raw_values"),
        MeanSquaredError(multioutput="raw_values", square_root=True),
    ]

    raw_metrics = atd2022.metrics.compute_metrics(
        true,
        preds.values(),
        metric_functions,
        models=preds.keys(),
        train=true,
        columnwise=True,
        verbose=True,
    )

    raw_metrics.index.name = "Team"
    raw_metrics.columns.name = "Unweighted Average"
    return raw_metrics


def column_wins(raw_metrics: pd.DataFrame) -> pd.Series:
    return pd.Series(
        (raw_metrics.rank(method="min").astype(int) == 1).sum(axis=1),
        name="# Column Wins",
    )


def _compute_metrics_for_one_code(
    truth: pd.DataFrame, predictions: pd.DataFrame, code: str
) -> pd.Series:
    metric_functions = [MeanAbsoluteScaledError()]
    event_code_slice = pd.IndexSlice[:, [code]]

    true_subset = truth.loc[:, event_code_slice]
    pred_subset = {
        model: pred.loc[:, event_code_slice] for model, pred in predictions.items()
    }

    return pd.Series(
        atd2022.metrics.compute_metrics(
            true_subset,
            pred_subset.values(),
            metric_functions,
            models=pred_subset.keys(),
            train=true_subset,
            verbose=False,
        )
        .sort_values(by="MeanAbsoluteScaledError")
        .iloc[:, 0],
        name=code,
    )


def compute_metrics_by_code(
    truth: pd.DataFrame, predictions: pd.DataFrame
) -> pd.DataFrame:
    metrics_by_code = pd.concat(
        [
            _compute_metrics_for_one_code(truth, predictions, f"{code:02}")
            for code in range(1, 21)
        ],
        axis=1,
    )
    metrics_by_code.columns.name = "Event"
    metrics_by_code.index.name = "Model"
    return metrics_by_code


def augment_name(codes: Sequence[str]) -> List[str]:
    events = Cameo.events_from_codes(codes)
    return [f"({code}) {event}" for code, event in zip(codes, events)]


def make_nstep_plot(metrics_n: pd.DataFrame) -> px.line:
    # Melt to flat dataframe
    df = metrics_n.reset_index().melt(["model", "n"], var_name="metric")

    fig = px.line(df, x="n", y="value", color="model", facet_col="metric")
    fig.update_yaxes(matches=None)
    return fig


def compute_nstep_metrics(
    truth: pd.DataFrame,
    predictions: Dict[str, pd.DataFrame],
) -> pd.DataFrame:
    metric_functions = [
        MeanAbsoluteScaledError(),
        MeanSquaredError(square_root=True),
    ]

    return atd2022.metrics.compute_nstep_metrics(
        truth,
        predictions.values(),
        metric_functions,
        models=predictions.keys(),
        train=truth,
        verbose=True,
    )


def create_site(
    metrics: pd.DataFrame,
    metrics_nstep: pd.DataFrame,
) -> pn.viewable.ServableMixin:
    title = pn.pane.Markdown("# ATD2022 Leaderboard")
    metrics_header = pn.pane.Markdown(
        """## Model Metrics

Table is sorted in increasing order of MASE. Better scores are colored in darker blue.

Metrics are described as follows:

* MASE - [Mean Absolute Scaled Error](https://en.wikipedia.org/wiki/Mean_absolute_scaled_error)
* RMSE - [Root Mean Squared Error](https://en.wikipedia.org/wiki/Root-mean-square_deviation)
* SPEC - [Stock-keeping-oriented Prediction Error Costs](https://arxiv.org/pdf/2004.10537.pdf)
* \# Column Wins - Compute both MASE and RMSE for each country/event individually (rather than as a single vector),
  then count the number of columns for which each forecaster has the best metric value (or ties).

Baseline models are prefixed with `BL`.
    """  # noqa
    )

    # Metrics table, styled static pandas version
    metrics_panel2 = pn.Row(
        pn.pane.HTML(
            (
                metrics.style.background_gradient(axis=0, cmap="PuBu_r")
                .background_gradient(
                    axis=0,
                    cmap="PuBu",
                    subset="# Column Wins",
                )
                .format(precision=2, subset=["MASE", "RMSE", "SPEC"])
                .render()
            )
        )
    )

    # Add nstep stuff
    metrics_n_header = pn.pane.Markdown(
        """## N-Step Metrics
        
In the below tables/plots, we compute metrics for each forecaster when partitioning the data by the prediction number.

Table is sorted by the same order as the above table. Better scores are colored in darker blue.
    """  # noqa
    )
    metrics_n_table = pn.Row(
        pn.pane.HTML(
            (
                metrics_nstep.loc[metrics.index]
                .style.background_gradient(
                    axis=0,
                    cmap="PuBu_r",
                )
                .format(precision=2)
                .render()
            )
        )
    )
    metrics_n_plot = pn.pane.Plotly(make_nstep_plot(metrics_nstep))
    column_stuff = [
        title,
        metrics_header,
        metrics_panel2,
        metrics_n_header,
        metrics_n_table,
        metrics_n_plot,
    ]
    return pn.Column(*column_stuff).servable(area="main")


def create_predictability_site(
    metrics_by_code: pd.DataFrame,
    event_rank_true: pd.Series,
    event_rank_preds: pd.DataFrame,
) -> pn.viewable.ServableMixin:
    title = pn.pane.Markdown("# ATD2022 Predictability Leaderboard")

    # Optionally, add by-code stuff
    metrics_by_code_header = pn.pane.Markdown(
        """## Predictability - MASE by Event Root Code

In Sprint 2, we asked teams to rank event codes in order of "most predictable" to "least predictable".

To make a vague concept like "predictability" measurable, we generate the ground-truth **predictability** as follows:

* Partition predictions/truth by event root code
* Compute MASE for each (forecaster, event root code) pair.
* Rank event root codes by the median MASE score (over the population of forecasters, including submissions and baselines).

### Predictability leaderboard

In the below table, we show the normalized [Kendall-Tau](https://en.wikipedia.org/wiki/Kendall_tau_distance) distance of each team's ranking.
"""  # noqa
    )

    metrics_by_code = metrics_by_code.copy()
    metrics_by_code_copy = metrics_by_code.copy()
    metrics_by_code.columns = augment_name(metrics_by_code.columns)

    metrics_box_plot_header = pn.pane.Markdown(
        """### Distribution of MASE by Event Code"""
    )

    TRUTH = event_rank_true.tolist()
    CMAP = get_cmap("PuBu_r", len(TRUTH))

    # Adapt pandas background_gradient to allow for a layer of indirection
    def relative_luminance(rgba: Sequence[float]) -> float:
        """
        Calculate relative luminance of a color.
        The calculation adheres to the W3C standards
        (https://www.w3.org/WAI/GL/wiki/Relative_luminance)
        Parameters
        ----------
        color : rgb or rgba tuple
        Returns
        -------
        float
            The relative luminance as a value from 0 to 1
        """
        r, g, b = (
            x / 12.92 if x <= 0.04045 else ((x + 0.055) / 1.055) ** 2.4
            for x in rgba[:3]
        )
        return 0.2126 * r + 0.7152 * g + 0.0722 * b

    def apply_gradient_color(val: Any) -> str:
        text_color_threshold = 0.408
        # Get background color
        rgba = CMAP(TRUTH.index(val))
        background_color = to_hex(rgba)

        # Get text color
        dark = relative_luminance(rgba) < text_color_threshold
        text_color = "#f1f1f1" if dark else "#000000"

        return f"background-color: {background_color};color: {text_color}"

    metrics_by_code_true_and_preds = pn.Row(
        pn.pane.HTML(
            pd.concat(
                [event_rank_true, event_rank_preds],
                axis=1,
            )
            .style.applymap(apply_gradient_color)
            .render()
        )
    )

    # Compute/viz KT
    import atd2022.metrics.ordinal

    kt_dist = pd.Series(
        {
            team: atd2022.metrics.ordinal.kendall_tau_distance(
                ranking.tolist(),
                event_rank_true,
                normalized=True,
            )
            for team, ranking in event_rank_preds.iteritems()
        },
        name="Normalized Kendall-Tau Distance",
    ).sort_values()
    kt_dist_pane = pn.pane.DataFrame(kt_dist)

    ax = px.box(
        metrics_by_code_copy,
        log_y=True,
        title="MASE by Event Codes (Lower is Better)",
    )

    metrics_by_code_box_plot_panel = pn.pane.Plotly(ax)

    true_preds_header = pn.pane.Markdown(
        """### Truth/Team Event Code Rankings

Columns are colored by the "true" order (with more predictable event codes being more blue).
"""  # noqa
    )

    column_stuff = [
        title,
        metrics_by_code_header,
        kt_dist_pane,
        metrics_box_plot_header,
        metrics_by_code_box_plot_panel,
        true_preds_header,
        metrics_by_code_true_and_preds,
    ]

    return pn.Column(*column_stuff).servable(area="main")
