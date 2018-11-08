#!/bin/bash
#SBATCH --partition=gpu8_medium
#SBATCH --nodes=2
#SBATCH --tasks-per-node=16
#SBATCH --cpus-per-task=1
#SBATCH --time=12:00:00
#SBATCH --mem=4G
#SBATCH --gres=gpu:8
#SBATCH --output=olfaction %j.log

module load r/3.5.0
Rscript RFandHier.R
