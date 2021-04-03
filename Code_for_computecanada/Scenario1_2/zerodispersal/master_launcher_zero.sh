#!/bin/bash
for i in `seq 1 2`;
do

sbatch --export=my_var=$i --job-name=BOA_calc_zero_$i --output=outputzero/output_BoAcalc_zero_$i.txt --error=errorzero/error_BoAcalc_zero_$i.txt BoA_forloopcalc_zero.sh
done