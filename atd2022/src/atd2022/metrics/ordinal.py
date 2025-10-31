import itertools
from collections import Counter
from typing import Collection

from sklearn.preprocessing import LabelEncoder


def kendall_tau_distance(
    order_a: Collection,
    order_b: Collection,
    normalized: bool = False,
) -> float:
    """Compute the Kendall-Tau distance between two orderings.

    ``order_a`` and ``order_b`` must contain exactly the same values.

    Parameters
    ----------
    order_a: Sequence
    order_b: Sequence
    normalized: bool
        Default False. If True, normalize distance between [0, 1].

    Returns
    -------
    float
    """
    encoder = LabelEncoder().fit(order_a)
    order_a = list(encoder.transform(order_a))
    order_b = list(encoder.transform(order_b))

    assert Counter(order_a) == Counter(order_b), "Input must contain same values."

    pairs = itertools.combinations(order_a, 2)
    distance = 0
    for x, y in pairs:
        a = order_a.index(x) - order_a.index(y)
        b = order_b.index(x) - order_b.index(y)
        if a * b < 0:
            distance += 1

    n = len(order_a)
    return distance if not normalized else distance / (n * (n - 1))
