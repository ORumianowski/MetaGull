#!/bin/sh
#SBATCH --job-name=Disper
#SBATCH -N 1
#SBATCH -n 3
#SBATCH --mem-per-cpu=5G
#SBATCH --partition=cefe

### Email
#SBATCH --mail-user=odin.rumianowski@cefe.cnrs.fr         
#SBATCH --mail-type=ALL

### Output & Error
#SBATCH --output=/lustre/rumianowskio/output/Disper.out
#SBATCH --error=/lustre/rumianowskio/output/Disper.err              


echo « Running on: $SLURM_NODELIST »

##load R
module load singularity

singularity exec R-4.3.2-equipe-HAIR-nobinding.img Rscript cmr_period2_v2_paramDisper_meso.R
