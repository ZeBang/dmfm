"""High-Level voting schemes for determining the best-performing Forecaster."""

import pandas as pd

__all__ = ["plurality", "instant_run_off"]


def plurality(metrics: pd.DataFrame) -> pd.Series:
    """Sort forecasters by the largest proportion of "most preferred" votes.

    Note
    ----
    Metrics are assumed to be "smaller is better".

    Parameters
    ----------
    metrics: pd.DataFrame
        Dataframe with forecasters as index and metrics as columns.

    Returns
    -------
    pd.Series
        Forecasters from index of input ``metrics``, sorted by their rank in
        plurality voting. The values in the series correspond to the fraction
        of all votes for which each forecaster was the "best".

    References
    ----------
    https://en.wikipedia.org/wiki/Plurality_voting
    """
    return (metrics.rank().astype(int) == 1).sum(axis=1).sort_values(
        ascending=False
    ) / metrics.shape[1]


def instant_run_off(metrics: pd.DataFrame) -> pd.Series:
    """Sort forecasters by their rank in an instant run off ranking scheme.

    This scheme progresses as follows:

    1. Each column is a voter, ranking their preferences from most preferred to least
       preferred.
    2. If any forecaster is the most preferred by greater than 50% of the columns, they
       win.
    3. Otherwise, drop the forecaster that has the fewest "most preferred" votes and
       repeat steps 2-3 until a winner is decided.

    Note
    ----
    Metrics are assumed to be "smaller is better".

    Parameters
    ----------
    metrics: pd.DataFrame
        Dataframe with forecasters as index and metrics as columns.

    Returns
    -------
    pd.Series
        Forecasters from index of input ``metrics``, sorted by their rank in
        the instant-run-off voting. The values in the series correspond to the either:

        1. the fraction of all votes for which each forecaster was the most-preferred
           forecaster at the time when a forecaster achieved majority.
        2. ``NaN``, if a forecaster was removed from consideration prior to a forecaster
           achieving majority.

    References
    ----------
    https://en.wikipedia.org/wiki/Instant-runoff_voting
    https://en.wikipedia.org/wiki/Ranked_voting
    """
    # Avoid mutating metrics outside of this function.
    metrics = metrics.copy()

    # Begin instant run off voting.
    removed_forecasters = []
    while True:
        # Compute plurality ranking
        ranking = plurality(metrics)
        # If we've achieved majority, stop. Otherwise drop forecaster with fewest
        #  most-preferred votes.
        if ranking.iloc[0] > 0.5:
            break
        else:
            least_preferred = ranking.idxmin()
            metrics.drop(least_preferred, inplace=True)
            removed_forecasters.append(least_preferred)

    # Append ``removed_forecasters`` to our ranking (with value NaN). Note that
    # ``removed_forecasters`` are ordered from least-preferred to more-preferred,
    # so we need to reverse it prior to concatenation.
    return pd.concat(
        [
            ranking,
            pd.Series(index=reversed(removed_forecasters), data=float("nan")),
        ]
    )
