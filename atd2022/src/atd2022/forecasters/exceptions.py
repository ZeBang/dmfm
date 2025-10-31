"""Forecaster exceptions."""
__all__ = ["NotFittedError", "UnsupportedPastCovariatesWarning"]


class NotFittedError(ValueError, AttributeError):
    """Exception class to raise if estimator is used before fitting.
    This class inherits from both ValueError and AttributeError to help with
    exception handling and backward compatibility.
    References
    ----------
    ..[1] Based on scikit-learn's NotFittedError
    """


class UnsupportedPastCovariatesWarning(RuntimeWarning):
    ...
