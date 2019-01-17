# SOS_scripts
Scripts associated with the creation of stimuli &amp; the presentation and analysis of behavioral and imaging tasks for the social/self stress sub-study within TAG.  

# Creating TIM video clips
Please see the video_instructions.txt file within the stimuli folder of this repository for step-by-step instructions on stimuli creation for this study.  Videos were recorded on an iMac using QuickTime Player Version 10.4 with the following settings.
  Format: H.264, 1280x720
          AAC 44100 Hz, Stereo (L R)
  FPS: 30

Video clips were created with the javascript file titled export_tim.jsx, which is available within the stimuli/ directory of this repository.  This script was launched through Adobe After Effects CC 2018, using Adobe's ExtendScript Toolkit.  Video clips were rendered via Adobe Media Encoder CC 2018.  

# Running fMRI task
This task was run using PsychoPy3 v3.0.0b7.  Trial order was determined using mutually orthogonal latin squares (MOLS) created using the R package crossdes.  Stimuli durations were determined by sampling from a gamma distribution with mean=16s for task blocks and mean=10s for rest blocks.  More detailed information is contained within the script titled Stim_Duration_Script_rArBrC.R, which is available within the stimuli/ directory of this repository.  This script was used to make stimulus order and timing decisions for the fMRI task.
