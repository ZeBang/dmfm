# atd2022
[![License](
    https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](
    https://opensource.org/licenses/BSD-3-Clause)
[![platform](
    https://img.shields.io/badge/platform-linux--64%20|%20win--64%20|%20osx--64-blue.svg)](
    https://img.shields.io/badge/platform-linux--64%20|%20win--64%20|%20osx--64-blue)
[![python](
    https://img.shields.io/badge/python-3.7|3.8|3.9-blue.svg)](
    https://img.shields.io/badge/python-3.7|3.8|3.9-blue)
[![Code style: black](
    https://img.shields.io/badge/code%20style-black-000000.svg)](
    https://github.com/psf/black)
[![Pipeline](
    https://gitlab.com/algorithms-for-threat-detection/2022/atd2022/badges/master/pipeline.svg)](
    https://gitlab.com/algorithms-for-threat-detection/2022/atd2022/pipelines)
[![ATD2022 Docs](
    https://img.shields.io/badge/atd2022_sphinx-online-green)](
    https://algorithms-for-threat-detection.gitlab.io/2022/atd2022/)
[![slack](
    https://img.shields.io/badge/slack-online-green)](
    https://atd2022.slack.com)

Welcome to the ATD2022 challenge!

In this README, we begin by defining this year's [challenge problem](#problem-description) and [dataset](#about-the-data). We continue by providing detailed instructions on how to [get started](#getting-started) by configuring your development environment. Finally, we describe [how to submit your code](#how-to-submit-your-code) to be evaluated on the hold out dataset. A list of [reference materials](#reference-material) is given in the FAQ section.

## Problem Description
The goal of this year's challenge is to create multivariate time series forecasters capable of predicting national-level geopolitical event counts.

Namely, the challenge will utilize the [GDELT dataset](https://www.gdeltproject.org/) which uses the [CAMEO coding system](http://data.gdeltproject.org/documentation/CAMEO.Manual.1.1b3.pdf) to record events and attribute them to organizations or state-actors. A subset of the events considered by the dataset include instances of "protests", "threats", "providing aid", "engaging in diplomatic cooperation", and "assaults".

In this challenge, we consider weekly event counts at a national level of fidelity. Participants will create multivariate time series forecasters that, given past history of event counts, predicts the number of events for each event type and for each country for the next `k` weeks. Models will be evaluated by backtesting the model on historical data, evaluated using standard time series metrics, and compared against baseline algorithms as well as the algorithms of their peers.

## ATD2022 Repository Contents

* `docs/` - Content used to generate the [ATD2022 API documentation](https://algorithms-for-threat-detection.gitlab.io/2022/atd2022/)
* `src/atd2022` - a python module containing the ATD2022 codebase.
* `tests/` - Unit tests you may run to ensure that you've installed `atd2022` correctly.
* `challenge.ipynb` - a walkthrough illustrating how to read in and view the data, make predictions using naive baseline forecasters, compute metrics, and analyze results.
* `LICENSE` - The license for the repository and its contents.
* `environment.yml` - Conda environment `yml` file used to install the `atd2022` dependencies.

In addition, there are several files related to the development, testing, and CI/CD of the repository. You can ignore the following files:

* `.gitignore`
* `.gitlab-ci.yml`
* `pyproject.toml`
* `setup.cfg`

## About the Data

The ATD 2022 Challenge dataset is national level, weekly aggregated event count data. The ATD 2022 Challenge dataset is derived from event data obtained from [The GDELT Project](https://www.gdeltproject.org/) Event Database version 2.0. The GDELT data contains records of events found in broadcast, print, and web news sources around the world and collated using deep learning algorithms. Each row in the GDELT Event Database corresponds to an event and each column corresponds to an event feature. Event features record where and when the event took place, the actors involved in the event, the source of the event information, and the type of event. Event types are recorded using the [Conflict and Mediation Event Observations (CAMEO) coding system](http://data.gdeltproject.org/documentation/CAMEO.Manual.1.1b3.pdf) to break down event types into hierarchical categories. The CAMEO event codes, ordered from least to most specific are a two-digit Event Root Code, a three-digit Event Base Code, and a four-digit Event Code. For more detail on the GDELT features, you can reference the [GDELT-Event_Codebook-V2.0 documentation](http://data.gdeltproject.org/documentation/GDELT-Event_Codebook-V2.0.pdf). These GDELT event records are aggregated to produce weekly time series of event counts for each of the 20 Event Root Codes at the national level for all world countries. Finally, we sequester a portion of this aggregated data into a holdout set that will be used to independently evaluate the performance of your submitted forecasters.

## Getting Started

### Outline

1. [Install tools](#install-tools)
1. (Optional) [Make Gitlab authentication convenient](#optional-make-gitlab-authentication-convenient)
1. [Clone atd2022](#clone-atd2022)
1. [Install atd2022](#install-atd2022)
1. [Open challenge.ipynb](#open-challenge-notebook)

### Install Tools

Install all of the following software:

* [`git`](https://git-scm.com/downloads)
    * If you are on Mac or Linux, you probably already have `git` installed.
* [`conda`](https://www.anaconda.com/distribution/) or [`mamba`](https://mamba.readthedocs.io/en/latest/installation.html).
    - `conda` is package and environment management system we will use for creating the `atd2022` development environment.
	- `mamba` is a drop-in replacement for `conda` that accelerates several, otherwise slow steps in environment creating process.

### (Optional) Make Gitlab Authentication Convenient

Because we are working in a private repository, Gitlab requires that you authenticate as your Gitlab user when performing server-side git commands (e.g., `clone`, `fetch`, `pull`, `push`).

We recommend that you utilize one of the following approaches to make authenticating to Gitlab more convenient.

1. ssh key-pair
	- Recommended if your local network's firewall allows ssh-tunneling to Gitlab
	- [Create an ssh key](https://gitlab.com/help/ssh/README#generating-a-new-ssh-key-pair)
	- [Upload it to your Gitlab user](https://gitlab.com/profile/keys)
1. [Git Credentials Manager](https://git-scm.com/book/en/v2/Git-Tools-Credential-Storage)
	- Recommend if your local network's firewall forbids ssh-tunneling
	- If you use `git config --global credential.helper cache` git will temporarily cache your username and password.
	- If you use `git config --global credential.helper store` git will permanently store your username and password in a plaintext file in your home directory
	- We recommend using whichever option fits your security requirements
	- If you do not use either of these approaches, then you will need to authenticate (by entering your username and password into your terminal) before every server-side command.

### Clone atd2022

If in [the previous section](#optional-make-gitlab-authentication-convenient) you elected to setup ssh-based authentication, run

```
git clone git@gitlab.com:algorithms-for-threat-detection/2022/atd2022.git
```

Otherwise, you can clone using

```
git clone https://gitlab.com/algorithms-for-threat-detection/2022/atd2022.git
```

If using the `https` address, you will need to enter your Gitlab username and password when prompted.

Once you clone `atd2022`, run `cd atd2022` to change directory into the `atd2022/` repository folder.


### Install atd2022

This section assumes you are using [`conda`](https://anaconda.org/anaconda/conda) or [`mamba`](https://mamba.readthedocs.io/en/latest/installation.html) to manage environments and packages.

In the previous section, you should have `git clone`d the `atd2022` repository and `cd`'d into the `atd2022/` repository folder.

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

You may verify that the `atd2022` package and it's dependencies installed successfully by running the unit test suite. Namely, from an activated `atd2022` environment and with your terminal's current directory set to the `atd2022/` root directory, you may run
```
pytest .
```

Create a kernel based on your `atd2022` environment by doing the following (with the `atd2022` conda environment active)
```
python -m ipykernel install --name atd2022 --user
```

This step will allow you to use the `atd2022` conda environment's software from within Jupyter notebooks using the `atd2022` kernel.

### Open Challenge Notebook

Once `atd2022` has been installed and you've created a conda kernel named `atd2022`, you are ready to open `challenge.ipynb` in your Jupyter Notebook server.

You can create a local Jupyter server by either using the Anaconda Navigator GUI or entering `jupyter notebook` into a conda-sourced terminal.

Please refer to the [Jupyter Notebook documentation](https://jupyter-notebook.readthedocs.io/en/stable/notebook.html#notebook-user-interface) if you are unfamiliar with the Jupyter Notebook user interface.

## Group Chat on the atd2022 Slack

You can use the ATD2022 [![slack](https://img.shields.io/badge/slack-online-green)](https://atd2022.slack.com) to chat with other participants and the ATD2022 administrative team in a persistent chat. As part of the the ATD2022 onboarding process, you should have received an email inviting you to join the `atd2022` slack group. Please reach out to challenge organizers through your PI if you did not receive this invitation.


## How to Submit Your Code

As part of the ATD2022 onboarding process, your team will receive a private Gitlab repository. To participate in the evaluation process, you will use `git` to push your forecaster code base to your team's repository. Following each Sprint submission deadline, the ATD2022 challenge administrators will checkout the master branch of your code and run it on the hold out dataset to evaluate your forecaster's performance.  

More information on how to use `git` and ensure the challenge adminstrators can clone and install your code will be forthcoming. In the meantime, some reference material is available [here](#Reference-Material).


## Calendar of Events
The ATD 2022 challenge consists of four Sprints, each of which includes optional informal WebEx Office Hours, a submission deadline, and a formal WebEx retrospective meeting.

### ATD 2022 Kickoff  
There will be an ATD 2022 WebEx kickoff meeting on Monday, May 2, 2022 from 2:00pm - 4:00pm. 

### Code Submission and Sprint Retrospectives  
Code should be submitted via your team's repository by midnight in your time zone for each sprint submission deadline, and all sprint retrospective meetings will be held from 1:00pm - 3:00pm Eastern Time. 

| Event    | Submission Deadline  | Retrospective Meeting |
| --------------- | --------------- | --------------- |
| Sprint 1 | Wednesday June 1, 2022 | Friday June 3, 2022|
| Sprint 2 | Monday July 11, 2022| Friday July 15, 2022|
| Sprint 3 | Monday August 22, 2022| Friday August 26, 2022|
| Sprint 4 | Monday September 19, 2022| Friday September 23, 2022|  


### Open Office Hours  
Open office hours will be held periodically throughout each sprint for drop in questions or discussion. There will always be an open office hour on the day of all sprint submission deadlines with a priority for submission issues. All office hour sessions will be held from 1:00pm - 2:00pm Eastern Time.

| Event    | Date  |
| --------------- | --------------- |
| Sprint 1 Office Hour 1 | Wednesday May 18, 2022 |
| Sprint 1 Office Hour 2 | Wednesday June 1, 2022 |
| Sprint 2 Office Hour 1 | Wednesday June 15, 2022 |
| Sprint 2 Office Hour 2 | Wednesday June 29, 2022 |
| Sprint 2 Office Hour 3 | Monday July 11, 2022 |
| Sprint 3 Office Hour 1 | Wednesday July 27, 2022 |
| Sprint 3 Office Hour 2 | Wednesday August 10, 2022 |
| Sprint 3 Office Hour 3 | Monday August 22, 2022 |
| Sprint 4 Office Hour 1 | Wednesday September 7, 2022 |
| Sprint 4 Office Hour 2 | Monday September 19, 2022 |
 

### ATD 2022 Google Calendar  
A Google Calendar with this event information including WebEx meeting information is available:

[Public calendar link for viewing ATD events](https://calendar.google.com/calendar/embed?src=rqomqt9h0j1n8gumscaujndh2k%40group.calendar.google.com&ctz=America%2FNew_York)

[iCal URL for adding the ATD calendar to your personal calendar app](https://calendar.google.com/calendar/ical/rqomqt9h0j1n8gumscaujndh2k%40group.calendar.google.com/public/basic.ics)  


## FAQ

### Reference Material

* The `git` book: https://git-scm.com/book/en/v2
* Concise `git` reference: https://www.atlassian.com/git/tutorials/atlassian-git-cheatsheet
* A simple `git` workflow to help you and your team collaborate: https://gist.github.com/jbenet/ee6c9ac48068889b0912

### How to Submit an Issue on Gitlab

For general questions, you can reach out the \#Help channel on the ATD2022 slack or message the challenge administrators.

However, if you encounter a particular bug in the `atd2022` code base that you believe should be resolved, you may submit an issue to the `atd2022` issue tracker. Please refer to [these instructions](https://docs.gitlab.com/ee/user/project/issues/) to learn how to submit issues in Gitlab.


### The `atd2022` codebase on Gitlab has updated since I first cloned the repository. How can I get the changes?

We can do this in three high level steps:

1. Ensure your working directory is clean.
    - Verify that there are no `staged` or `unstaged` changes when running `git status` (untracked are probably OK).
1. Pull the changes from Gitlab into your current branch in your local repository
    - `git pull --rebase origin master`
    - This will ensure that you have the most recent data, notebooks, and source files in your `atd2022` directory.
1. To update the ATD Python packages and dependencies do one of the following:
    1. Install just `atd2022` and its dependencies.
        - This should be the procedure you try first, as it will allow you to keep any packages you've installed since your initial installation.
        - To update `atd2022`, run `pip install -e .[dev]` (with your terminal's current directory set to the `atd2022/` repository root directory).
    1. Reinstall the entire `atd2022` conda environment
      - This is the "nuclear option" and will over-write any packages you've manually installed in this environment after your initial installation.
      - If Windows or on a JupyterLab server, be sure to do the following first to avoid the Windows filesystem locking files that require re-installation:
          - `conda deactivate` to the base environment
          - Close all applications/tasks (besides your current conda prompt) that may have a lock on any file in your `atd2022` conda environment
              - `jupyter-notebook` tasks, `jupyter notebook` browser windows, running kernels, windows explorer open to the `atd2022` conda environment directory, etc.
      - From the `atd2022` repository folder, run `conda env create -f environment.yaml --force`
      - `conda activate atd2022`

# We hope you enjoy the ATD 2022 Challenge!
