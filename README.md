# IllusoryPitch

This repository contains the stimuli, experimental code, raw data, and analysis files for the manuscript:

Pazdera, J. K., Rinaldi, O. M., & Trainor, L. J. (2024). Timing-induced illusory percepts of pitch. _PsyArXiv_.

The project was originally shared as a preprint on PsyArXiv/OSF:
<https://osf.io/preprints/psyarxiv/caxsb>

If a final published version becomes available, it can be linked here alongside the preprint.

## Repository contents

**E1/** contains the browser-based implementation of Experiment 1, the pre-generated trial schedules, the raw participant exports, and the analysis notebooks/scripts used for the first experiment.

**E2/** contains the PsychoPy implementation of Experiment 2, the generated stimuli used for adaptive testing and the main task, the raw participant exports, and the analysis notebooks/scripts used for the second experiment.

Across both experiments, the repository includes:

- raw participant-level CSV files
- processed summary tables used for the manuscript analyses
- Jupyter notebooks for stimulus preparation, preprocessing, and figure generation
- R scripts for confirmatory statistical analyses
- exported manuscript figures in PDF/SVG format

Detailed documentation is provided in:

- [E1/README.md](E1/README.md)
- [E2/README.md](E2/README.md)

## Software overview

The repository combines several toolchains:

- **Experiment 1** was implemented in **jsPsych** for online deployment via **Pavlovia**.
- **Experiment 2** was implemented in **PsychoPy**.
- **Stimulus generation and figure notebooks** use Python/Jupyter packages such as `numpy`, `pandas`, `scipy`, `matplotlib`, `seaborn`, `librosa`, and `soundfile`.
- **Statistical analyses** use R packages including `tidyverse`, `sjstats`, `lme4`, `lmerTest`, and related plotting/modeling packages.

Some analysis scripts contain machine-specific `setwd(...)` paths from the original project environment, so those paths may need to be updated before rerunning the analyses on a new machine.

## Reproducibility notes

- The repository preserves both raw and processed data products.
- Confirmatory analysis scripts encode the participant exclusions and version filters used for the manuscript.
- Review-oriented notebooks are retained in `analysis/review/` within each experiment directory.

## License

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
