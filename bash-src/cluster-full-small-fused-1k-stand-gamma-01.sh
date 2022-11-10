#!/bin/bash

#SBATCH --partition=priority
#SBATCH --qos=priority
#SBATCH --constraint=haswell
#SBATCH --job-name=full-small-fused-1k-stand-gamma-01
#SBATCH --account=flai
#SBATCH --output=out/%x/o-%j.out
#SBATCH --error=out/%x/e-%j.err
#SBATCH --workdir=/home/lepke
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8


source setup.txt
Rscript code/R/fused-lasso/small-fused-1k-stand-gamma-01/fit-full-small-fused-1k-stand-gamma-01.R
# hostnamepp