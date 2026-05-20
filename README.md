# IllusoryPitch

This repository contains the stimuli, experimental code, raw data, and analysis files for the manuscript:

Pazdera, J. K., Rinaldi, O. M., & Trainor, L. J. (2026). Timing-induced illusory percepts of pitch. _PsyArXiv_.

A preprint of the manuscript can be found at the following link, pending its upcoming publication in _Scientific Reports_:
<https://osf.io/preprints/psyarxiv/caxsb_v3>

## Contents

**E1/** contains all stimuli, data, and code for Experiment 1. \
**E2/** contains all stimuli, data, and code for Experiment 2.

For each experiment, the repository includes:

- jsPsych / PsychoPy code and stimuli used to run the study
- Raw, participant-level CSV data files
- Processed data tables used for the analyses in the manuscript
- Jupyter (Python) notebooks for stimulus preparation, preprocessing, and figure generation
- R scripts for statistical analyses
- Exported manuscript figures in PDF/SVG format

Experiment-specific documentation is provided in:

- [E1/README.md](E1/README.md)
- [E2/README.md](E2/README.md)

Please note that some analysis scripts contain machine-specific file paths from the original project environment (e.g., `setwd(...)`), so those paths may need to be updated before rerunning the analyses on a new machine.

## License

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
