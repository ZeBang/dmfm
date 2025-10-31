## Repository Contents

* `atd2022` [atd2022](https://gitlab.com/algorithms-for-threat-detection/2022/atd2022/) module usd to fit, predict and load data
* `DMFM_paper.ipynb` code to address comments.
* `ATD2022_DynamicMatrixFactorModel.ipynb` DynamicMatrixFactorModel.
* `ATD2022_AveragingModel_final_version.ipynb` Ensemble model of the final submissions.
* `models.py` DynamicMatrixFactorModel and EnsembleModel python scrypts.
* `ATD_helper.r` DynamicMatrixFactorModel R scrypts.

## Install atd2022

This section assumes you are using [`conda`](https://anaconda.org/anaconda/conda) or [`mamba`](https://mamba.readthedocs.io/en/latest/installation.html) to manage environments and packages.

To install all dependencies for `atd2022`, in your anaconda sourced terminal and from within the `atd2022/` repository folder you may do one of the following. If you have `mamba`, enter

```
mamba env create -f environment.yml
```

Alternatively, if you only have `conda` available, enter

```
conda env create -f environment.yml
```

After creating your environment, you can activate the environment by running :

```
conda activate atd2022
```


Create a kernel based on your `atd2022` environment by doing the following (with the `atd2022` conda environment active)
```
python -m ipykernel install --name atd2022 --user
```

This step will allow you to use the `atd2022` conda environment's software from within Jupyter notebooks using the `atd2022` kernel.

Once `atd2022` has been installed and you've created a conda kernel named `atd2022`, you are ready to open `DMFM_paper.ipynb` in your Jupyter Notebook server.

You can create a local Jupyter server by either using the Anaconda Navigator GUI or entering `jupyter notebook` into a conda-sourced terminal.
