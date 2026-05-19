# Experiment 1 (E1)

## Summary

`E1/` contains the materials for the first experiment reported in the manuscript. This experiment was run online using **jsPsych** with **Pavlovia** integration and tests whether temporal expectancy biases judgments of whether a final probe tone is higher or lower in pitch.

The main task code defines:

- participant IDs from `1` to `300`
- `4` experimental blocks
- `60` main trials per block
- a short practice phase before the main task

## Directory contents

### Core experiment files

- `index.html` loads the jsPsych experiment in a browser.
- `experiment.js` contains the experiment timeline, instructions, trial logic, metadata fields, and Pavlovia save/init calls.

### Stimuli and schedules

- `stimuli/` contains the WAV files used in the task.
- `stimuli/tones/` contains the base tone files used to build the stimulus sequences.
- `schedules/` contains `session1.json` through `session300.json`, which define pre-generated trial orders for each participant ID.

### Preparation notebooks

- `preparation/Tone_Generation (IP).ipynb` generates the stimulus tones/sequences.
- `preparation/Trial_Randomization (IP).ipynb` generates the session schedules used by the online experiment.

### Data files

`data/` contains both raw exports and processed data products:

- `Illusory_Pitch_PARTICIPANT_SESSION_*.csv`: raw participant files exported from the online study
- `response_data.csv`: trial-level processed responses used for downstream analyses
- `scores.csv`: condition-level sensitivity/bias summaries
- `subj_scores.csv`: subject-level summary scores used for correlational analyses

## Analysis files

`analysis/` contains the code used for preprocessing, visualization, and confirmatory statistics:

- `Processing (IP).ipynb`: preprocessing from raw data to analysis-ready tables
- `Analysis (IP).ipynb`: figure generation and exploratory plotting
- `Stats (IP).R`: confirmatory statistics reported for Experiment 1
- `figures/`: exported PDF/SVG figures
- `review/`: additional notebooks prepared for review-stage sensitivity/bias checks

The R script currently excludes subjects `13`, `15`, `22`, `31`, and `35`, matching the confirmatory analysis decisions encoded in the repository.

## Running or reusing the experiment

- The browser task expects jsPsych 6.1/Pavlovia library assets referenced from `index.html`.
- Those third-party library files are not included in this repository, so relaunching the task may require restoring the corresponding `lib/` directory from the original experiment environment or from a fresh Pavlovia/jsPsych setup.
- For analysis reruns, update any machine-specific working-directory paths before executing the R scripts.

## Software dependencies

- **Browser task:** jsPsych, jQuery, Pavlovia plugin
- **Python/Jupyter notebooks:** `numpy`, `pandas`, `scipy`, `matplotlib`, `seaborn`, `librosa`, `soundfile`
- **R analysis:** `sjstats`, `dplyr`, `tidyverse`, `lme4`, `lmerTest`, `car`
