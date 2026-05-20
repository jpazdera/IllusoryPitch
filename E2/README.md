# Experiment 2 (E2)

## Summary

`E2/` contains the materials for for Experiment 2, as reported in the manuscript. This experiment was run locally in **PsychoPy** and combines an adaptive staircase estimate of pitch discrimination threshold with the main timing-manipulated, pitch perception task.

## Contents

### Core experiment file

- `experiment_ip_adaptive.py` contains the PsychoPy task, including the staircase calibration procedure, dynamic stimulus creation, and the control and logging of the main experiment.

### Stimuli

- `preparation/Tone Generation (IP-AD).ipynb` contains the notebook used to generate the base tones for the staircase procedure.
- `stimuli/tones/` contains the base tone library for the staircase procedure.
- `stimuli/S*/` contains generated sequence sets and `tone_log.json` files for individual subjects. These are the custom-generated stimuli produced by `experiment_ip_adaptive.py` after the staircase procedure.

### Data files

`data/` contains both raw exports and processed data products:

- `IPAD_*.csv`: Raw participant data output by PsychoPy.
- `response_data.csv`: Trial-level processed responses used for analyses.
- `scores.csv`: Condition-level sensitivity/bias summaries for each subject.
- `fits.csv`: Timing-induced bias data, characterized by the slope and intercept of lines fit across their data in different timing conditions.
- `exploratory_scores.csv`: Additional exploratory summary table, not used in analyses.

### Analysis files

`analysis/` contains preprocessing notebooks, figure notebooks, and confirmatory statistical scripts:

- `Processing (IP-AD).ipynb`: Preprocesses from raw data to analysis-ready tables. Includes response scoring and calculation of signal detection theory measures, as well as the calculation of timing-induced bias.
- `Analysis (IP-AD).ipynb`: Contains performance screening and figure generation.
- `Cue Integration.ipynb`: Contains simulation of the effects of pitch shift size versus noise on bias and sensitivity, as shown in Discussion section.
- `MixedEffectE2.R`: Contains all statistical analyses. Note that participant 21 is excluded due to failure to perform above chance.
- `figures/`: Folder containing exported PDF/SVG figures
- `review/`: Folder containing additional notebooks used by Olive Rinaldi to review each participant's data for outliers

## Software dependencies

- **PsychoPy experiment:** `psychopy`, `numpy`, `librosa`, `soundfile`
- **Python/Jupyter notebooks:** `numpy`, `pandas`, `scipy`, `matplotlib`, `seaborn`
- **R analysis:** `tidyverse`, `sjstats`, `sjPlot`, `lme4`, `lmerTest`
