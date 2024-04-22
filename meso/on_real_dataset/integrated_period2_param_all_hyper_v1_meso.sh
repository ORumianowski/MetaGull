#!/bin/sh
#SBATCH --job-name=integrated_all_hyper_v1
#SBATCH -N 1
#SBATCH -n 3
#SBATCH --mem-per-cpu=5G
#SBATCH --partition=cefe

### Email
#SBATCH --mail-user=odin.rumianowski@cefe.cnrs.fr         
#SBATCH --mail-type=ALL

### Output & Error
#SBATCH --output=/lustre/rumianowskio/output/int_v14_v1.out
#SBATCH --error=/lustre/rumianowskio/output/int_v14_v1.err              


echo « Running on: $SLURM_NODELIST »

##load R
module load singularity

singularity exec R-4.3.2-equipe-HAIR-nobinding.img Rscript integrated_period2_param_all_hyper_v1_meso.R
