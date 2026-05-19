# Experiment 2 (E2)

## Summary

`E2/` contains the materials for the second experiment reported in the manuscript. This experiment was run locally in **PsychoPy** and combines an adaptive staircase estimate of pitch discrimination threshold with the main timing-manipulation task.

The main experiment script defines:

- an interleaved adaptive staircase for estimating each participant's JND
- `4` main-task blocks
- fully randomized repetitions of shift direction, timing interval, and difficulty
- generated participant-specific stimulus sets saved under `stimuli/S*/`

## Directory contents

### Core experiment file

- `experiment_ip_adaptive.py` contains the PsychoPy task, including staircase calibration, stimulus playback, trial flow, response collection, logging, and data saving.

### Stimulus generation

- `preparation/Tone Generation (IP-AD).ipynb` contains the notebook used to generate tones/sequences for the adaptive experiment.
- `stimuli/tones/` contains the base tone library for the staircase and sequence generation.
- `stimuli/S*/` contains generated sequence sets and `tone_log.json` files for individual subjects or sessions.

### Data files

`data/` contains both raw exports and processed data products:

- `IPAD_*.csv`: raw participant exports from the PsychoPy task
- `response_data.csv`: trial-level processed responses
- `scores.csv`: condition-level summaries used for the primary analyses
- `fits.csv`: fitted summary parameters including intercepts/slopes for timing-induced bias
- `exploratory_scores.csv`: additional exploratory summary table used in later analyses

## Analysis files

`analysis/` contains preprocessing notebooks, figure notebooks, and confirmatory statistical scripts:

- `Processing (IP-AD).ipynb`: preprocessing from raw exports to analysis-ready tables
- `Analysis (IP-AD).ipynb`: main figures and descriptive analyses
- `Cue Integration.ipynb`: supplementary cue-integration analysis
- `MixedEffectE2.R`: confirmatory statistical analyses for Experiment 2
- `figures/`: exported PDF/SVG figures
- `review/`: additional review-stage notebooks

The confirmatory R script filters out subject `21` and restricts some analyses to recordings with `version >= 1.1`, matching the analysis decisions encoded in the repository.

## Running or reusing the experiment

- The PsychoPy task depends on `psychopy`, `numpy`, `librosa`, `soundfile`, and standard Python libraries used by the script.
- The script writes logs and data files relative to the experiment directory, so it should be run from within `E2/` or with paths adjusted accordingly.
- For analysis reruns, update any machine-specific working-directory paths before executing the R scripts.

## Software dependencies

- **PsychoPy experiment:** `psychopy`, `numpy`, `librosa`, `soundfile`
- **Python/Jupyter notebooks:** `numpy`, `pandas`, `scipy`, `matplotlib`, `seaborn`
- **R analysis:** `tidyverse`, `sjstats`, `sjPlot`, `lme4`, `lmerTest`
