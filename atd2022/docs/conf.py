# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
from typing import List

sys.path.insert(0, os.path.abspath("../../src"))


# -- Project information -----------------------------------------------------

project = "ATD2022"
copyright = "2022, National Geospatial-Intelligence Agency (NGA)"
author = "Doug Mercer"


# -- General configuration ---------------------------------------------------
autoclass_content = "both"
autodoc_default_options = {
    # Make sure that any autodoc declarations show the right members
    "members": True,
    "inherited-members": True,
    "private-members": True,
    "show-inheritance": True,
}
autodoc_default_flags = [
    "members",
    "inherited-members",
    "private-members",
    "special-members",
    "show-inheritance",
]
autosummary_default_options = {
    "nosignatures": True,
}

extensions = [
    "sphinx_rtd_theme",
    "sphinx.ext.napoleon",
    "sphinx.ext.autodoc",
    "sphinx.ext.autosummary",
    "sphinx_autodoc_typehints",
    # "sphinx.ext.intersphinx",
]

autoclass_content = "both"
autodoc_typehints = "description"
autodoc_typehints_format = "short"
autodoc_typehints_description_target = "documented"
napoleon_type_aliases = {
    "pd.DataFrame": ":class:`~pandas.DataFrame`",
    "Iterable": ":class:`~typing.Iterable`",
    "Iterable[pd.DataFrame]": ":class:`~typing.Iterable`\[:class:`~pandas.DataFrame`\]",  # noqa: W605, E501
    "list[pd.DataFrame]": ":class:`list`\[:class:`~pandas.DataFrame`\]",  # noqa: W605, E501
    "Iterator[memoryview]": ":class:`~typing.Iterator`\[:class:`memoryview`\]",  # noqa: W605, E501
}
autosummary_generate = True
napoleon_use_param = True
napoleon_use_rtype = True
todo_include_todos = True
napoleon_preprocess_types = True
intersphinx_mapping = {
    "python": ("https://docs.python.org/3", None),
    "numpy": ("http://docs.scipy.org/doc/numpy", None),
    "pandas": ("https://pandas.pydata.org/pandas-docs/stable/", None),
}

# Add any paths that contain templates here, relative to this directory.
templates_path = ["_templates"]

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns: List[str] = [".ipynb_checkpoints"]


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = "sphinx_rtd_theme"


# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ["_static"]
