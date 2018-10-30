#!/bin/bash
#SBATCH -p gpu8_medium
#SBATCH --nodes=1
#SBATCH --tasks-per-node=16
#SBATCH --cpus-per-task=1
#SBATCH --time=2-00:00:00
#SBATCH --mem-per-cpu=4G
#SBATCH --gres=gpu:8

module load r/3.5.0
Rscript RFandHier.R
