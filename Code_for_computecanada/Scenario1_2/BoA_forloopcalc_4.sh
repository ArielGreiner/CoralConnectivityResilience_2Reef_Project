#!/bin/bash

#SBATCH --mail-user=ariel.kiara@gmail.com
#SBATCH --mail-type=BEGIN 
#SBATCH --mail-type=END 
#SBATCH --mail-type=FAIL 
#SBATCH --mail-type=REQUEUE 
#SBATCH --nodes=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=96G
#SBATCH --time=0-1:00

module load arch/avx2 StdEnv/2016.4

module load nixpkgs/16.09 gcc/7.3.0 r/3.6.0 

parallel --compress --colsep="," -j 40 /home/agreiner/scratch/BoA_calculations/run_BoAcalc_code_SMALL2.sh :::: "/home/agreiner/scratch/BoA_calculations/iteratortry3/BOA_iteratortry3_$my_var.csv" 
