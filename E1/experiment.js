/* - - - - LOAD ID - - - - */
var urlvars = jsPsych.data.urlVariables();
var minID = 1;
var maxID = 300;
var subjectID;

 // If participant has a valid ID, use it, otherwise assign a random ID
if (('participant' in urlvars) && !(isNaN(parseInt(urlvars['participant']))) &&
(parseInt(urlvars['participant']) >= minID) && (parseInt(urlvars['participant']) <= maxID)) {
    subjectID = parseInt(urlvars['participant']);
} else {
    subjectID = Math.floor(Math.random() * (maxID - minID + 1)) + minID;
}

var audio_test_score = 0;

// Add session metadata
// v1.0 was used in the study
// v1.1 has improved practice trial text (changed "final tone began" -> "final tone was")
jsPsych.data.addProperties({
    subject: subjectID,
    experiment: 'IP',
    code_version: 'v1.1'
});

// Use participant ID number to load the trial order, and don't run anything else until it's done loading
$.getJSON(`schedules/session${subjectID}.json`).done(function (schedule) {

    /* - - - - SETTINGS - - - - */

    var n_blocks = 4
    var trials_per_block = 60;
    var post_instruction_delay = 2000;
    var post_response_delay = 1500;

    /* - - - - PAVLOVIA INTEGRATION - - - - */

    var pavlovia_init = {
        type: 'pavlovia',
        command: 'init'
    };

    var pavlovia_finish = {
        type: 'pavlovia',
        command: 'finish'
    };

    /* - - - - INSTRUCTIONS - - - - */

    // Welcome message
    var welcome = {
        type: 'html-keyboard-response',
        data: { event: 'welcome' },
        stimulus: '<p>Welcome to the study! Press any key to begin reading the instructions.</p>'
    };
    
    // Main task instructions (pre-practice)
    var instructions_main = {
        type: 'html-button-response',
        data: { event: 'main_instructions' },
        choices: ['Start'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<p><strong>Instructions:</strong> Today we\'ll be asking you to listen to sequences of tones ' +
        'and to make some judgments about them. Specifically, you will hear seven tones on each trial. The first ' +
        'six tones you hear will be identical to one another, but the seventh tone will be shifted slightly higher ' +
        'or lower in pitch. Your goal is to determine whether the final tone is higher or lower than the first ' +
        'six. You will need to listen carefully, as the change in pitch will be small. We are interested in the ' +
        'limits of human abilities, so don\'t feel discouraged if you find it difficult to tell the tones apart.</p>' +
        
        '<p>Let\'s try a few practice trials to help you get used to the task. Click the button below when you are ' +
        'ready to start practicing.</p>'
    };

    // Final main task instructions (post-practice)
    var instructions_final = {
        type: 'html-button-response',
        data: { event: 'summary_instructions' },
        choices: ['Start'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<p>Great work! Now you\'re ready to try it for real.</p>' +
        
        '<p>Please note that the trials will be organized into ' + (n_blocks).toString() + ' sections. Each ' +
        'section will last about 5 minutes, and you will be able to take a break after each. Remember, your goal is ' +
        'to determine whether the final tone on each trial was higher or lower in pitch than the first six.</p>' +
        
        '<p>Press the button below to begin.</p>'
    };
        
    // Completion screen
    var ending = {
        type: 'html-button-response',
        data: { event: 'ending' },
        choices: ['Submit'],
        stimulus: '<h3>You have completed section ' + (n_blocks).toString() + ' of ' + (n_blocks).toString() + '!</h3>' +
        
        '<p>Thank you for participating! Please let the researcher know that you have finished.</p>' +
        
        '<p><em>Have a great day!</em></p>'
    };
    
    // Debriefing form
    var debrief = {
        type: 'html-button-response',
        data: { event: 'debrief' },
        choices: ['Exit'],
        stimulus: '<h1>About This Study</h1>' +
        '<h3>Principal Investigator: Dr. Laurel J. Trainor (ljt@mcmaster.ca)</h3>' +
        '<h3>Researcher: Jesse K. Pazdera, B.Sc. (pazderaj@mcmaster.ca)</h3>' +
        '<p style="text-align: justify; text-indent: 35px; max-width: 750px;">In this study, you listened to ' +
        'sequences of tones and rated whether the final tone was higher or lower in pitch than the others. The ' +
        'first six tones were identical in pitch and played at a steady rate, whereas the final tone was shifted up ' +
        'or down in pitch very slightly. The final tone either played on time, 15% early, or 15% late. Past ' +
        'research from our lab and others has suggested that human perception is most accurate when we hear sounds ' +
        'at the expected time, and this expectancy may help us to better understand speech and to better enjoy ' +
        'music. Our first hypothesis, then, is that it will be easier to follow the change in pitch of the final ' +
        'tone when that tone plays on time, and harder when the final tone plays early or late. We are also ' +
        'interested, however, in whether the timing of a sound can bias our brains to perceive it as higher or ' +
        'lower in pitch than it actually was. Previous studies have suggested that people tend to misperceive lower ' +
        'pitched sounds as slower than those that are higher pitched. We are curious whether the reverse effect ' +
        'will also occur - whether auditory timing influences perceived pitch. Specifically, our second hypothesis is ' +
        'that people will misperceive sounds as higher in pitch when those sounds are earlier than expected, and ' +
        'will misperceive them as lower in pitch when the sounds are later than expected. By testing your ability ' +
        'to detect small changes in pitch under different timing conditions, we can determine how timing biases ' +
        'your auditory perception. Your participation will help us to better understand how our perception of sound ' +
        'and time interact.' +
        '<h3 style="max-width: 750px;">We would like to thank you again for participating in our study. If you ' +
        'would like to learn more about our lab\'s research, please contact Jesse Pazdera at the email listed ' +
        'above. Additionally, we ask that you do not share or discuss any information about the purpose of this ' +
        'study with your classmates who may wish to participate in the future.</h3>' 
    };

    /* - - - - PRE-TASKS - - - - */

    // Practice Trials
    var practice_trials = {
        timeline: [
            // Tone presentation
            {
                type: 'audio-keyboard-response',
                data: {
                    event: 'practice_tones',
                    octave: jsPsych.timelineVariable('octave'),
                    pitch_shift: jsPsych.timelineVariable('shift'),
                    offset: jsPsych.timelineVariable('offset')
                },
                stimulus: jsPsych.timelineVariable('stimulus'),
                response_ends_trial: false,
                trial_ends_after_audio: true
            },
            // Response screen
            {
                type: 'html-keyboard-response',
                data: {
                    event: 'practrice_response',
                    octave: jsPsych.timelineVariable('octave'),
                    pitch_shift: jsPsych.timelineVariable('shift'),
                    offset: jsPsych.timelineVariable('offset')
                },
                stimulus: 'Was the final tone (up arrow) higher or (down arrow) lower in pitch?',
                choices: ['downarrow', 'uparrow']
            },
            // Feedback for practice
            {
                type: 'html-keyboard-response',
                data: { event: 'practice_feedback' },
                response_ends_trial: true,
                stimulus: jsPsych.timelineVariable('answer'),
                post_trial_gap: post_response_delay
            }
        ],
        timeline_variables: [
            {stimulus: `stimuli/sequence_A4-_500_0.wav`, octave: 4, shift: '-', offset: 0, answer: '<strong>Answer:</strong> The final tone was <strong>lower</strong> on this trial. Press any key to continue.'},
            {stimulus: `stimuli/sequence_A4+_500_0.wav`, octave: 4, shift: '+', offset: 0, answer: '<strong>Answer:</strong> The final tone was <strong>higher</strong> on this trial. Press any key to continue.'},
            {stimulus: `stimuli/sequence_A4-_500_0.wav`, octave: 4, shift: '-', offset: 0, answer: '<strong>Answer:</strong> The final tone was <strong>lower</strong> on this trial. Press any key to continue.'},
            {stimulus: `stimuli/sequence_A4+_500_0.wav`, octave: 4, shift: '+', offset: 0, answer: '<strong>Answer:</strong> The final tone was <strong>higher</strong> on this trial. Press any key to continue.'}
        ],
        randomize_order: true
    }
    
    // Build list of audio files that will need to be loaded
    var audio_files = new Set();
    var practice_strings = ['A4+_500_0', 'A4-_500_0']
    for (i in practice_strings) {
        s = practice_strings[i];
        audio_files.add(`stimuli/sequence_${s}.wav`);
    }

    /* - - - - BLOCKING - - - - */

    // Add instructions, audio test, SPR test and practice trials to timeline
    var timeline = [pavlovia_init, welcome, instructions_main, practice_trials, instructions_final];

    // Dynamically construct trials based on schedule, while adding each audio file to the preload list
    for (block = 0; block < n_blocks; block++) {
        for (trial = 0; trial < trials_per_block; trial++) {
            // Add stimulus to audio file list
            audio_files.add(`stimuli/sequence_A${schedule[block][trial][0]}${schedule[block][trial][1]}_500_${schedule[block][trial][2]}.wav`);
            
            // Stimulus presentation event
            timeline.push({
                type: 'audio-keyboard-response',
                stimulus: `stimuli/sequence_A${schedule[block][trial][0]}${schedule[block][trial][1]}_500_${schedule[block][trial][2]}.wav`,
                response_ends_trial: false,
                trial_ends_after_audio: true,
                data: {
                    event: 'tones',
                    octave: `${schedule[block][trial][0]}`,
                    pitch_shift: `${schedule[block][trial][1]}`,
                    offset: `${schedule[block][trial][2]}`
                },
            });
            // Response screen event
            timeline.push({
                type: 'html-keyboard-response',
                stimulus: 'Was the final tone (up arrow) higher or (down arrow) lower in pitch?',
                choices: ['downarrow', 'uparrow'],
                post_trial_gap: post_response_delay,
                data: { 
                    event: 'response',
                    octave: `${schedule[block][trial][0]}`,
                    pitch_shift: `${schedule[block][trial][1]}`,
                    offset: `${schedule[block][trial][2]}`
                },
            });
        }
        // Break period
        if (block < n_blocks - 1) {
            timeline.push({
                type: 'html-button-response',
                data: { event: 'break' },
                choices: ['Continue'],
                post_trial_gap: post_instruction_delay,
                stimulus: '<h3>You have completed section ' + (block + 1).toString() + ' of ' + (n_blocks).toString() +
                '!</h3><p>When you are ready to continue, press the button below to begin the next section.</p>'
            });
        }
    }

    // Set up experiment conclusion
    timeline.push(ending);
    timeline.push(pavlovia_finish);
    timeline.push(debrief);

    audio_files = Array.from(audio_files)

    /* - - - - EXECUTION - - - - */

    jsPsych.init({
        timeline: timeline,
        default_iti: 0,
        use_webaudio: true,
        preload_audio: audio_files,
        show_preload_progress_bar: true,
        show_progress_bar: true,
        exclusions: {audio: true}
    });
});