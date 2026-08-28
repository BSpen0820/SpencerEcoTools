#!/bin/bash
#SBATCH --job-name=Endotherm_Model
#SBATCH --output=logs/Endotherm_Model_%A/Endotherm_Model_%A_%a.out
#SBATCH --error=logs/Endotherm_Model_%A/Endotherm_Model_%A_%a.err
#SBATCH --nodes=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=185G
#SBATCH -p reg
#SBATCH --array=1-50   # <-- change range to however many tasks you want

sleep $((SLURM_ARRAY_TASK_ID * 30))
echo "Running task $SLURM_ARRAY_TASK_ID at $(date)"

set -euo pipefail
module purge

source ~/miniconda3/etc/profile.d/conda.sh
conda activate apptainer_env

cd "$PROJECT_ROOT"
mkdir -p logs

export HOST_TMPDIR="$PROJECT_ROOT/temp/job_${SLURM_JOB_ID}_array_${SLURM_ARRAY_TASK_ID}"
mkdir -p "$HOST_TMPDIR" "$HOST_TMPDIR/xdg_cache"

cleanup_tmp=1
trap 'cleanup_tmp=0; exit 143' TERM INT
trap 'if [ "$cleanup_tmp" -eq 1 ]; then rm -rf -- "$HOST_TMPDIR"; else echo "Cancelled: retaining $HOST_TMPDIR for safe cleanup."; fi' EXIT

apptainer exec --no-mount home,cwd --pwd /temp \
  --env TMPDIR=/temp --env TMP=/temp --env TEMP=/temp \
  --env XDG_CACHE_HOME=/temp/xdg_cache \
  --env ENDO_WINEPREFIX=/temp/wine/prefix \
  --env PROJECT_ROOT=/project \
  --bind "$PROJECT_ROOT":/project \
  --bind "$HOST_TMPDIR":/temp \
  nichemapdockerenv.sif mamba run -n r_micro Rscript /project/R_code/Endotherm_LandscapeScale.R \
    --clust_array_arg=$SLURM_ARRAY_TASK_ID \
    --clust_array_size=$SLURM_ARRAY_TASK_COUNT \
    --n_threads=$SLURM_CPUS_PER_TASK

echo "Task ${SLURM_ARRAY_TASK_ID} completed successfully."
