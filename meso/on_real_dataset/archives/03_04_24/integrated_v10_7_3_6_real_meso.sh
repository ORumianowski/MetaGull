#!/bin/sh
#SBATCH --job-name=v10_7_3_6
#SBATCH -N 1
#SBATCH -n 3
#SBATCH --mem-per-cpu=5G
#SBATCH --partition=cefe

### Email
#SBATCH --mail-user=odin.rumianowski@cefe.cnrs.fr         
#SBATCH --mail-type=ALL

### Output & Error
#SBATCH --output=/lustre/rumianowskio/output/v10_7_3_6.out
#SBATCH --error=/lustre/rumianowskio/output/v10_7_3_6.err              


echo « Running on: $SLURM_NODELIST »

##load R
module load singularity

singularity exec R-4.3.2-equipe-HAIR-nobinding.img Rscript integrated_v10_7_3_6_real_meso.R
