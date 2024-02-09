import json
import itertools
import os
import random
import librosa as lba
import numpy as np
import soundfile as sf
from psychopy import constants, core, data, event, gui, logging, prefs, sound, visual

prefs.general['units'] = 'pix'
prefs.general['fullscr'] = False
prefs.general['allowGUI'] = True


def generate_percussive_tone(freq, duration, rise_duration, perc_duration, fade_duration, sr=44100):

    rise_length = int(rise_duration * sr / 1000)
    perc_length = int(perc_duration * sr / 1000)
    fade_length = int(fade_duration * sr / 1000)

    # Generate fundamental frequency (co)sine wave
    tone = lba.tone(freq, sr=sr, duration=duration / 1000)

    # Add first three harmonics (up to 2 octaves above) with slope of -3 db/half amplitude per octave
    for i in range(3):
        phase = random.random() * 2 * np.pi
        tone += lba.tone(freq * (i + 2), sr=sr, duration=duration / 1000, phi=phase) / (i + 2)

    # Apply exponential fade to create percussive envelope
    tone[-perc_length:] *= np.geomspace(1, .01, perc_length)
    # Apply short linear fade to ending so that amplitude fades to 0
    tone[-fade_length:] *= np.linspace(1, 0, fade_length)
    # Apply sharp linear rise to start of tone
    tone[:rise_length] *= np.linspace(0, 1, rise_length)

    # Rescale waveform to range [-1, 1]
    tone /= np.abs(tone).max()

    return tone


def generate_sequences(base_freq, tone_name, base_shift, difficulties, ioi, offsets,
                       base_intervals=5, mod_intervals=1, sr=44100,
                       tone_dir='stimuli/tones/', out_dir='stimuli/'):

    if not os.path.exists(out_dir):
        os.mkdir(out_dir)

    ntones = base_intervals + mod_intervals + 1

    # Load standard tone
    base_tone, _ = lba.load(tone_dir + 'tone%s.wav' % tone_name, sr=sr)

    # Calculate frequencies of probe tones based on JND
    tones = dict()
    for d in difficulties:
        tones['%s-%s' % (tone_name, d)] = base_freq / 2 ** (base_shift * d / 1200)
        tones['%s+%s' % (tone_name, d)] = base_freq * 2 ** (base_shift * d / 1200)

    for pitch in tones:

        freq = tones[pitch]
        tone = generate_percussive_tone(freq, duration=250, rise_duration=10, perc_duration=240, fade_duration=10)

        for offset in offsets:

            # Create array of appropriate length to hold audio sequence
            ms_ioi = offset / 1000  # Millisecond interval preceding probe tone
            sequence = np.zeros(int(np.ceil(ntones * max(ioi / 1000, ms_ioi) * sr)), dtype=np.float32)

            # Insert tones at appropriate locations
            for i in range(ntones):

                # First several tones are spaced by the base IOI
                if i <= base_intervals:
                    start = i * ioi / 1000 * sr
                    start = int(np.ceil(start))

                    # Place a copy of the tone in the proper location
                    base_tone_end = start + base_tone.shape[0]
                    sequence[start:base_tone_end] = base_tone

                # Final tone(s) is/are preceded by the modified IOI
                else:
                    start = sr * ((base_intervals * ioi / 1000) + ((i - base_intervals) * ms_ioi))
                    start = int(np.ceil(start))

                    # Add pitch-shifted tone after the standard tones
                    tone_end = start + tone.shape[0]
                    sequence[start:tone_end] = tone

            # Cut silence from the end of the sequence
            sequence = np.trim_zeros(sequence, 'b')

            # Save sequences to WAV file
            sf.write(out_dir + 'sequence_%s_%i.wav' % (pitch, offset), sequence, sr)

    # Log information about the tones generated
    tones['base_shift_cents'] = base_shift
    tones['difficulties'] = difficulties
    with open(out_dir + 'tone_log.json', 'w') as f:
        json.dump(tones, f)


###
# INITIALIZATION
###

# VERSION NUMBERS
# 1.0 - Difficulty levels of JND vs. 3/2 * JND
# 1.1 - Increased difficulty to 1/2 JND vs. JND

# Set constants

# Experiment settings
experiment_name = 'IPAD'  # Experiment name
version_num = '1.1'  # Experiment version number (see above)
frame_rate = 60  # Set monitor frame rate

# Staircase settings
min_shift = 0  # Minimum pitch shift in cents during staircase
start_shift_small = 1  # The initial pitch shift in cents to use on small-start staircases
start_shift_large = 25  # The initial pitch shift in cents to use on large-start staircases
max_shift = 100  # Maximum pitch shift in cents during staircase
n_up = 1  # Number of incorrect responses required to get easier
n_down = 2  # Number of correct responses required to get harder
reversals_per_staircase = 8  # The total number of reversals each staircase should be run for
reversals_to_use = 4  # The last N reversals will be averaged to estimate the JND; even numbers reduce estimation bias
step_sizes = [8, 8, 4, 4, 2, 2, 1, 1]  # The step sizes for the staircase; can change after each reversal

# Main task settings
pretrial_delay = 1.5  # Seconds of pause before each trial
standard_freq = 440  # Frequency of the standard tone (in Hz)
tone_name = 'A4'  # Name of the standard tone
standard_ioi = 500  # Standard inter-onset interval (in ms)
shifts = ['+', '-']  # Pitch directions
intervals = [425, 500, 575]  # Length of the interval preceding the probe tone
difficulties = [.5, 1]  # Pitch between high and low probes (in multiples of the JND)
repetitions_per_block = 5  # Repetitions of each condition within each block (fully randomized)
blocks = 4  # Number of blocks to run
n_practice_trials_per_shift = 2  # Number of practice trials per shift direction
practice_interval = standard_ioi  # Interval preceding probe on practice trials
practice_difficulty = 4  # Difference between high and low probes on practice trials (in multiples of the JND)

# Randomize trial order
# Practice trials include 2 up and 2 down shifts at the expected onset time and easy difficulty
# Main trials include 4 blocks with 5 repetitions of each condition (2 shifts x 3 intervals x 2 difficulties) per block
conditions = [c for c in itertools.product(shifts, intervals, difficulties)]
trials_per_block = len(conditions) * repetitions_per_block  # Number of trials per block
trial_order = []
for i in range(n_practice_trials_per_shift):
    for shift in shifts:
        trial_order.append({'shift': shift, 'interval': practice_interval, 'difficulty': practice_difficulty, 'event': 'practice'})
random.shuffle(trial_order)  # Shuffle practice trials
for block in range(blocks):
    block_trials = conditions * repetitions_per_block
    random.shuffle(block_trials)
    for trial in block_trials:
        trial_order.append({'shift': trial[0], 'interval': trial[1], 'difficulty': trial[2], 'event': 'trial'})

# Set up session info and open dialogue box to enter participant ID
info_dict = dict(subject='', experimenter='')
dlg = gui.DlgFromDict(dictionary=info_dict, sortKeys=False, title=experiment_name)
if not dlg.OK:
    core.quit()
info_dict['experiment'] = experiment_name
info_dict['version'] = version_num

# Set logging
log = logging.LogFile('logs/%s_%s.log' % (experiment_name, info_dict['subject']), level=logging.EXP)
logging.console.setLevel(logging.EXP)

# Set up experiment, window, and text object
win = visual.Window([1920, 1080], screen=0, monitor=None, color=(-1, -1, -1), fullscr=True)
text = visual.TextStim(win, '', font='Arial', color=(1, 1, 1), height=72)
exp = data.ExperimentHandler(name=experiment_name, version=version_num,
                             extraInfo=info_dict,
                             dataFileName='data/%s_%s_bkp' % (experiment_name, info_dict['subject']),
                             savePickle=False, saveWideText=True,
                             autoLog=True, appendFiles=False)

# Set up interleaved staircase procedure used for difficulty calibration
stair_conditions = [
    # Staircase testing JND for pitch increases starting from large shift
    {
        'label': 'l+', 'startVal': start_shift_large, 'nReversals': reversals_per_staircase, 'stepSizes': step_sizes,
        'nUp': n_up, 'nDown': n_down, 'minVal': min_shift, 'maxVal': max_shift, 'stepType': 'lin', 'shift': '+'
    },
    # Staircase testing JND for pitch increases starting from small shift
    {
        'label': 's+', 'startVal': start_shift_small, 'nReversals': reversals_per_staircase, 'stepSizes': step_sizes,
        'nUp': n_up, 'nDown': n_down, 'minVal': min_shift, 'maxVal': max_shift, 'stepType': 'lin', 'shift': '+'
    },
    # Staircase testing JND for pitch decreases starting from large shift
    {
        'label': 'l-', 'startVal': start_shift_large, 'nReversals': reversals_per_staircase, 'stepSizes': step_sizes,
        'nUp': n_up, 'nDown': n_down, 'minVal': min_shift, 'maxVal': max_shift, 'stepType': 'lin', 'shift': '-'
    },
    # Staircase testing JND for pitch decreases starting from small shift
    {
        'label': 's-', 'startVal': start_shift_small, 'nReversals': reversals_per_staircase, 'stepSizes': step_sizes,
        'nUp': n_up, 'nDown': n_down, 'minVal': min_shift, 'maxVal': max_shift, 'stepType': 'lin', 'shift': '-'
    }
]
adaptive_stairs = data.MultiStairHandler(stairType='simple', method='random', conditions=stair_conditions, nTrials=1)
exp.addLoop(adaptive_stairs)

# Set up main task
trials = data.TrialHandler(trial_order, 1, method='sequential',
                           dataTypes=['event', 'shift', 'interval', 'difficulty',
                                      'response', 'rt', 'jnd', 'full_jnd', 'correct'])
exp.addLoop(trials)

###
# ADAPTIVE DIFFICULTY TEST
###

# Set up for loudness calibration
standard_tone = sound.Sound('stimuli/tones/tone%s.wav' % tone_name)
text.setText('Which was higher?')
text.draw()
win.flip()
event.waitKeys(keyList=['space'], clearEvents=True)

for intensity, condition in adaptive_stairs:

    # Pre-trial delay; load tones and randomize order while waiting
    win.flip()
    pretime = core.StaticPeriod(screenHz=frame_rate, win=win)
    pretime.start(pretrial_delay)
    shift = condition['shift']
    if intensity == 0:
        probe_tone = sound.Sound('stimuli/tones/tone%s.wav' % tone_name)
    else:
        probe_tone = sound.Sound('stimuli/tones/tone%s%s%d.wav' % (tone_name, shift, intensity))
    pretime.complete()
    
    # Stimulus presentation
    standard_tone.play()
    core.wait(0.5)
    probe_tone.play()

    # Ask for participant response
    text.setText('Which was higher?')
    text.draw()
    win.flip()
    response = event.waitKeys(keyList=['1', '2'], clearEvents=True)

    # Score correctness, and automatically reverse direction if we reach 0 intensity
    if intensity == 0:
        correctness = False
    else:
        correctness = ('2' in response and shift == '+') or ('1' in response and shift == '-')
    adaptive_stairs.addResponse(correctness)
    exp.nextEntry()

# Set the JND to the average across the final N reversals of all staircases
text.setText('Thinking...')
text.draw()
win.flip()
reversals = []
for staircase in adaptive_stairs.staircases:
    reversals += staircase.reversalIntensities[-reversals_to_use:]
JND = np.mean(reversals)

# Generate tone sequences based on the individual's JND
generate_sequences(standard_freq, tone_name, JND, difficulties + [practice_difficulty], standard_ioi, intervals,
                   base_intervals=5, mod_intervals=1, sr=44100,
                   tone_dir='stimuli/tones/', out_dir='stimuli/S%s/' % info_dict['subject'])

###
# MAIN TASK
###

text.setText('Up or Down?')
text.draw()
win.flip()
event.waitKeys(keyList=['space'], clearEvents=True)
win.flip()

# Loop through trials
trial_number = 1  # Start on trial 1
block_number = 0  # Start on block 0 (practice section)
for trial in trials:

    ###
    # PRETRIAL
    ###

    # Load stimulus sequence for this trial during the pretrial delay
    pretime = core.StaticPeriod(screenHz=frame_rate, win=win)
    pretime.start(pretrial_delay)
    shift = trial.shift
    interval = trial.interval
    difficulty = trial.difficulty
    stimulus = sound.Sound('stimuli/S%s/sequence_%s%s%s_%i.wav' % (info_dict['subject'], tone_name, shift, difficulty, interval))
    text.setText('+')
    text.draw()
    pretime.complete()

    # Display fixation cross and play the simulus sequence
    win.flip()
    stimulus.play()

    # Wait for the sequence to end
    text.setText('Was the final tone higher or lower in pitch?')
    text.draw()
    core.wait(3)
    while not stimulus.status == constants.FINISHED:
        core.wait(.01)

    # Prompt for the participant's response and map up/down to +/-
    win.flip()
    response_period_start = core.getAbsTime()
    response = event.waitKeys(keyList=['up', 'down'], clearEvents=True)
    rt = core.getAbsTime() - response_period_start
    if 'up' in response:
        response = '+'
    elif 'down' in response:
        response = '-'
    else:
        response = None
    win.flip()

    # Save data
    trials.addData('shift_size', JND * difficulty)
    trials.addData('response', response)
    trials.addData('rt', rt)
    trials.addData('jnd', JND)
    trials.addData('correct', int(shift == response))
    exp.nextEntry()

    ###
    # END OF PRACTICE
    ###

    # If this was the final trial of the practice block (0), end the practice
    if block_number == 0 and trial_number == n_practice_trials_per_shift * len(shifts):
        text.setText('You have completed the practice trials!')
        text.draw()
        win.flip()
        event.waitKeys(keyList=['space'], clearEvents=True)
        block_number += 1
        trial_number = 0

    ###
    # POST-BLOCK BREAK
    ###

    # If this was the final trial in a block, start a break
    elif trial_number == trials_per_block and block_number != blocks:
        text.setText('You have completed section %i of %i!\nWhen you are ready to continue, press SPACEBAR to begin the next section.' % (block_number, blocks))
        text.draw()
        win.flip()
        event.waitKeys(keyList=['space'], clearEvents=True)
        block_number += 1
        trial_number = 0

    # If this was the final trial in the last block we have time for, stop presenting trials
    elif trial_number == trials_per_block and block_number == blocks:
        break

    # Move to next trial number
    trial_number += 1

###
# POST-EXPERIMENT
###

# After completing all trials, display the ending message
text.setText('You have completed section %i of %i!\nThank you for participating! Please let the researcher know you have finished.' % (block_number, blocks))
text.draw()
win.flip()

# Data should save automatically, but manually save a backup copy just in case
exp.saveAsWideText('data/%s_%s.csv' % (experiment_name, info_dict['subject']))

# Press any key to close the window, then exit
event.waitKeys(clearEvents=True)
win.close()
core.quit()
