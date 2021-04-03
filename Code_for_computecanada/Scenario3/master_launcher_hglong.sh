#!/bin/bash
for i in `seq 1 495`;
do

sbatch --export=my_var=$i --job-name=BOA_calc_hg_$i --output=outputhg_redo/output_BoAcalc_hglong_$i.txt --error=errorhg_redo/error_BoAcalc_hglong_$i.txt BoA_forloopcalc_hglong.sh
done