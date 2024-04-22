#!/bin/sh
#SBATCH --job-name=v12_prior_p_hyper2
#SBATCH -N 1
#SBATCH -n 3
#SBATCH --mem-per-cpu=5G
#SBATCH --partition=cefe

### Email
#SBATCH --mail-user=odin.rumianowski@cefe.cnrs.fr         
#SBATCH --mail-type=ALL

### Output & Error
#SBATCH --output=/lustre/rumianowskio/output/v12_prior_p_hyper2.out
#SBATCH --error=/lustre/rumianowskio/output/v12_prior_p_hyper2.err              


echo « Running on: $SLURM_NODELIST »

##load R
module load singularity

singularity exec R-4.3.2-equipe-HAIR-nobinding.img Rscript semi_cmr_AE_v12_prior_p_hyper2_meso.R
