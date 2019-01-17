#!/bin/bash
#
# This script runs FreeSurfer 6.0 recon-all
# and extracts the data, which includes the
# all morphological measurements at vertex
# and parcellated scales. Run this script
# by running the batch_reconall.sh file
# and calling on subject_list.txt
#

echo -e "\nSetting Up Freesurfer6.0"

source /projects/dsnlab/shared/sos/SOS_scripts/sMRI/SetUpFreeSurfer.sh 

echo -e "\nFreesurfer Home is $FREESURFER_HOME"

echo -e "\nThe Subject Directory is $SUBJECTS_DIR"

echo -e "\Running recon-all on ${SUBID}"

recon-all -i /projects/dsnlab/shared/sos/bids_data/"${SUBID}"/ses-sos/anat/"${SUBID}"_ses-sos_T1w.nii.gz -subjid "${SUBID}" -all
