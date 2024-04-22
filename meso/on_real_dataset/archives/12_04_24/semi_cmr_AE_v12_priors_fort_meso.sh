#!/bin/sh
#SBATCH --job-name=v12_priors_fort
#SBATCH -N 1
#SBATCH -n 3
#SBATCH --mem-per-cpu=5G
#SBATCH --partition=cefe

### Email
#SBATCH --mail-user=odin.rumianowski@cefe.cnrs.fr         
#SBATCH --mail-type=ALL

### Output & Error
#SBATCH --output=/lustre/rumianowskio/output/v12_priors_fort.out
#SBATCH --error=/lustre/rumianowskio/output/v12_priors_fort.err              


echo « Running on: $SLURM_NODELIST »

##load R
module load singularity

singularity exec R-4.3.2-equipe-HAIR-nobinding.img Rscript semi_cmr_AE_v12_priors_fort_meso.R
