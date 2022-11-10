#!/bin/bash

#SBATCH --partition=priority
#SBATCH --qos=priority
#SBATCH --constraint=haswell
#SBATCH --job-name=large-fused-10k
#SBATCH --account=flai
#SBATCH --output=out/%x/o-%j.out
#SBATCH --error=out/%x/e-%j.err
#SBATCH --workdir=/home/lepke
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16


source setup.txt
Rscript code/R/fused-lasso/large-fused-10k/cv-large-fused-10k.R
# hostname