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

### Analysis files

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

## Raw data codebook
The following are the columns present in the ITM_*.csv raw data files.

- **trial_type:** The jsPsych trial type of the current event.
- **trial_index:** The event index. "Trial index" is a bit of a misnomer, as it includes all events, not only experiment trials.
- **time_elapsed:** The number of milliseconds elapsed from the time the experiment began to the end of the current event.
- **internal_node_id:** jsPsych's internal event-tracking numbers.
- **subject:** The subject number of the participant that produced the data.
- **experiment:** Abbreviated experiment name. Always IP for Experiment 1, standing for "Illusory Pitch".
- **code_version:** Would be used to track which version of the code was used for a given participant. Always v1.0 for Experiment 1.
- **rt:** The number of milliseconds between the onset of the event's screen and the participant's key- or button-press response.
- **stimulus:** Indicates either the text that was displayed onscreen or the sound file that was played.
- **key_press:** Indicates the JavaScript key code of the key the participant pressed on that trial. For the main task, 38 = up arrow (a "higher" response) and 40 = down arrow (a "lower" response).
- **event:** Indicates the type of trial/event/screen that row of data was logged from.
  - tones / practice_tones: Presentation of one sequence of standard tones followed by a probe tone. Practice trials include the word "practice" in the event name.
  - response / practice_response: Screen prompting a "higher"/"lower" resonse. Practice trials include the word "practice in the event name.
  - practice_feedback: Screen providing feedback as to whether the probe was higher or lower. Feedback was only given on practice trials.
  - welcome/break/ending: Welcome, mid-experiment break, and ending screens.
  - main_instructions / summary_instructions: Screens with task instructions prior to the experiment.
- **button_pressed:** Indicates the participant pressed a button onscreen. Typically associated with instruction screens.
- **octave:** Indicates which octave the trial was in. Either 3 (A3) or 5 (A5) for the main task, and 4 (A4) for the practice trials.
- **pitch_shift:** Indicates whether the probe tone was higher (+) or lower (-) than the standard tones.
- **offset:** The timing offset of the probe tone, expressed as a percent of the interonset interval (which was 500 ms). Can either be -15 (15% early), 0 (on time), or 15 (15% late).

## Processed data codebook
The following are the columns that exist in the processed data files, but were not in the raw data files. Descriptions of columns present in the raw data can be found above.

- **answer:** The correct answer as to whether the probe tone was higher than the standard tones. False if the pitch shift was "-", True if the pitch shift was "+".
- **response:** The participant's answer to the question of whether the probe tone was higher than the standard tones. False if the participant responded "lower", True if the participant responded "higher".
- **correct:** Indicates whether the participant's response was correct. False if no, True if yes.
- **hit_rate:** The participant's hit rate, where a hit indicates that they correctly identified a high-pitched probe as high.
- **fa_rate:** The participant's false alarm rate, where a false alarm indicates that they incorrectly rated a low-pitched probe as high.
- **accuracy:** The proportion of probes the participant correctly responded to.
- **perc_resp_low:** The proportion of probes the participant rated as low-pitched.
- **dprime:** The participant's _d'_ sensitivity, based on their hit rate and false alarm rate.
- **C:** The participant's bias, _C_, based on their hit rate and false alarm rate. Positive _C_ indicates a bias to rate probes as low-pitched, while negative _C_ indicates a bias to rate probes as high-pitched.
- **C_slope / C_intercept:** The participant's timing-induced bias, expressed as the slope and intercept of their bias across timing conditions.
