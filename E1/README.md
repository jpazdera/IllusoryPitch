# Experiment 1 (E1)

## Summary

`E1/` contains the materials for Experiment 1, as reported in the manuscript. This experiment runs online through **Pavlovia** as a **jsPsych** study and tests whether temporal expectancy biases judgments of whether a final probe tone is higher or lower in pitch. (Note that although the experiment is browser-based, we conducted the study in-lab, with participants accessing **Pavlovia** on a lab computer. The browser-based format acted as a fail-safe in case COVID restrictions interrupted in-person research.)

## Contents

### Core experiment files

- `index.html` loads the jsPsych experiment in a browser. Note that the code was written to use Pavlovia's built-in library assets, so these are not included with the codebase. 
- `experiment.js` contains the experiment timeline, instructions, trial logic, metadata fields, and Pavlovia calls.

### Stimuli and schedules

- `stimuli/` contains the WAV files of tone sequences used in the task. File naming is of the form: `sequence_[PITCH][PITCH CHANGE]_[INTERONSET INTERVAL]_[PROBE TIMING].wav`. For example, an early pitch decrease in the third octave would be `sequence_A3-_500_-15.wav`.
- `stimuli/tones/` contains the base, individual tone files used to build the stimulus sequences. Files ending in `-normed.wav` are the versions normalized to be the same loudness on the headphones used in our study; those without this tag are the pre-normalized raw waveforms.
- `schedules/` contains `session1.json` through `session300.json`, which define pre-generated trial orders for each participant ID (up to 300).

### Preparation notebooks

- `preparation/Tone_Generation (IP).ipynb` generates the stimulus tones/sequences found in `stimuli/`.
- `preparation/Trial_Randomization (IP).ipynb` generates the session schedules used by the online experiment, as found in `schedules/`.

### Data files

`data/` contains both raw exports and processed data products:

- `Illusory_Pitch_PARTICIPANT_SESSION_*.csv`: Raw participant files output by Pavlovia.
- `response_data.csv`: Trial-level processed responses used for analyses.
- `scores.csv`: Condition-level sensitivity/bias summaries for each subject.
- `subj_scores.csv`: Subject-level summary scores used for correlation analyses.

## Analysis files

`analysis/` contains the code used for preprocessing, visualization, and statistics:

- `Processing (IP).ipynb`: Preprocesses from raw data to analysis-ready tables. Includes response scoring and calculation of signal detection theory measures.
- `Analysis (IP).ipynb`: Contains performance screening and figure generation.
- `Stats (IP).R`: Contains all statistics. Currently set to exclude participants 13, 15, 22, 31, and 35 based on their failure to perform above chance identified in `Analysis (IP).ipynb`
- `figures/`: Folder containing exported PDF/SVG figures
- `review/`: Folder containing additional notebooks used by Olive Rinaldi to review each participant's data for outliers

## Software dependencies

- **Browser task:** jsPsych, jQuery, Pavlovia plugin
- **Python/Jupyter notebooks:** `numpy`, `pandas`, `scipy`, `matplotlib`, `seaborn`, `librosa`, `soundfile`
- **R analysis:** `sjstats`, `dplyr`, `tidyverse`, `lme4`, `lmerTest`, `car`
