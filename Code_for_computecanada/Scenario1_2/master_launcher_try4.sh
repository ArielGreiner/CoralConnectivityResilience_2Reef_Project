#!/bin/bash
for i in `seq 1 2`;
do

sbatch --export=my_var=$i --job-name=BOA_calc_4_$i --output=outputtry4/output_BoAcalc_try4_$i.txt --error=errortry4/error_BoAcalc_try4_$i.txt BoA_forloopcalc_4.sh
done