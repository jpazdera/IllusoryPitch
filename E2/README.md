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

## Raw data codebook
The following are the columns present in the ITM_*.csv raw data files. Note that the data files are formatted somewhat differently from Experiment 1 due to the switch from jsPsych + Pavlovia to PsychoPy.

- **shift:** Indicates whether the probe tone was higher (+) or lower (-) than the standard tones.
- **interval:** The interonset interval preceding the probe tone, in milliseconds. Either 425 (early), 500 (on time), or 575 (late).
- **difficulty:** The number of JNDs by which the probe tone differed from the standard tones. Only accurate for version 1.1 (see **version** notes below).
- **event:** Indicates the type of trial that row of data was logged from.
  - practice: A practice trial including a sequence of standard tones followed by a probe and a response prompt.
  - trial: A main experiment trail consisting of a sequence of standard tones followed by a probe and a response prompt.
  - <None>: Any events that were part of the staircase procedure at the start of the session did not have an event tag.
- **.thisRepN:** Unused, always zero.
- **.thisTrialN:** The trial number. Does not count the staircase procedure trials.
- **.thisN:** Same as **.thisTrialN** and **.thisIndex**.
- **.thisIndex:** Same as **.thisTrialN** and **.thisN**.
- **.label:** For the interleaved staircase procedure, indicates which staircase generated the trial. There were four staircases, two starting from large pitch changes (l) and two starting from small pitch changes (s); two that generated pitch increases (+) and two that generated pitch decreases (-).
- **.startVal:** For the interleaved staircase procedure, indicates the current staircase's original number of cents the tones differed by. Always 1 or 25.
- **.nReversals:** Always 8, indicating that each staircase ran for 8 reversals during the staircase procedure.
- **.stepSizes:** Always `[8, 8, 4, 4, 2, 2, 1, 1]`, indicating that the step sizes shrunk by half every two reversals in the staircase procedure.
- **.nUp:** Always 1, indicating the staircase procedure used a 1-up, 2-down approach.
- **.nDown:** Always 2, indicating the staircase procedure used a 1-up, 2-down approach.
- **.minVal:** Always 0, indicating that the smallest cent difference in the staircase procedure is to play two identical tones.
- **.maxVal:** Always 100, indicating that the cent difference in the staircase procedure can never exceed 100 (one semitone).
- **.stepType:** Always 'lin', indicating the interleaved staircases use linear steps.
- **.shift:** For the interleaved staircase procedure, indicates whether the second tone on a trial was higher (+) or lower (-) than the first.
- **.direction:** For the interleaved staircase procedure, indicates whether that trial's staircase became more difficult (down) or less difficult (up) since its last trial.
- **.stepSize:** For the interleaved staircase procedure, indicates the step size on a particular trial.
- **.intensity:** For the interleaved staircase procedure, indicates the number of cents by which the two tones differed on a particular trial.
- **.response:** For the interleaved staircase procedure, indicates whether the participant responded correctly (True) or incorrectly (False).
- **shift_size:** For main task trials, the number of cents by which the probe tone differed from the standard tones (i.e., difficulty * jnd). Only accurate for version 1.1 (see **version** notes below).
- **response:** Indicates whether the participant responded that the probe tone was higher (+) or lower (-) than the standard tones.
- **rt:** The number of seconds after the probe tone ended that the participant responded. Note that a bug caused reaction times to be rounded to the nearest second in Experiment 2.
- **jnd:** The participant's just-noticeable difference in cents, as determined by the staircase procedure at the start of the session.
- **correct:** For main task trials, indicates whether the participant's response was correct (1) or incorrect (0).
- **subject:** The participant's subject ID number.
- **experimenter:** The initials of the experimenter that ran the session.
- **experiment:** The abbreviated experiment name. Always IPAD for Experiment 2, standing for "Illusory Pitch: Adaptive Difficulty". 
- **version:** Indicates the version of the experiment code used.
  - 1.0: No difficulty manipulation due to a string formatting bug that caused both the 1.5 and 1 JND difficulty conditions to load the 1 JND difficulty files. This version comprises the participants that only received the 1.0 JND difficulty condition in the manuscript.
  - 1.1: Fixed difficulty manipulation and increased difficulty to 0.5 JND vs. 1 JND. This version comprises the main participants reported in the manuscript.

## Processed data codebook
The following are the columns that exist in the processed data files, but were not in the raw data files. Descriptions of columns present in the raw data can be found above.

- **answer:** The correct answer as to whether the probe tone was higher than the standard tones. False if the pitch shift was "-", True if the pitch shift was "+".
- **response:** The participant's answer to the question of whether the probe tone was higher than the standard tones. False if the participant responded "lower", True if the participant responded "higher".
- **correct:** Indicates whether the participant's response was correct. False if no, True if yes.
- **offset:** Indicates the percent timing offset of the probe tone. Can be -15 (15% early), 0 (on time), or 15 (15% late).
- **hit_rate:** The participant's hit rate, where a hit indicates that they correctly identified a high-pitched probe as high.
- **fa_rate:** The participant's false alarm rate, where a false alarm indicates that they incorrectly rated a low-pitched probe as high.
- **accuracy:** The proportion of probes the participant correctly responded to.
- **perc_resp_low:** The proportion of probes the participant rated as low-pitched.
- **dprime:** The participant's _d'_ sensitivity, based on their hit rate and false alarm rate.
- **C:** The participant's bias, _C_, based on their hit rate and false alarm rate. Positive _C_ indicates a bias to rate probes as low-pitched, while negative _C_ indicates a bias to rate probes as high-pitched.
- **C_slope / C_intercept:** The participant's timing-induced bias, expressed as the slope and intercept of their bias across timing conditions.
