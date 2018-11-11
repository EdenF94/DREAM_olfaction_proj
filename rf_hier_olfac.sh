#!/bin/bash
#SBATCH --partition=gpu8_medium
#SBATCH --nodes=1
#SBATCH --tasks-per-node=16
#SBATCH --cpus-per-task=1
#SBATCH --time=12:00:00
#SBATCH --mem-per-cpu=4G
#SBATCH --gres=gpu:8
#SBATCH --output=olfaction_rf_hc_%j.log

module load r/3.5.0
Rscript RF_and_HierClust.R
