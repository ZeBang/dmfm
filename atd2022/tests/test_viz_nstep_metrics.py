import pandas as pd
import pytest
import seaborn as sns

import atd2022


@pytest.mark.xfail
def test_plot_nstep_metrics() -> None:
    df = pd.DataFrame(
        index=pd.MultiIndex.from_product(
            (range(10), list("abc")), names=["model", "n"]
        ),
        columns=["my_metric"],
    )

    assert isinstance(atd2022.viz.plot_nstep_metrics(df), sns.FacetGrid)
