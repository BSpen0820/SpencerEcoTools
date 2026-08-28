#!/bin/bash
#SBATCH --job-name=Endotherm_Model_v2
# `logs` must exist before sbatch is called. Slurm opens these files before
# this script can create directories.
#SBATCH --output=logs/Endotherm_Model_v2/Endotherm_Model_v2_%A_%a.out
#SBATCH --error=logs/Endotherm_Model_v2/Endotherm_Model_v2_%A_%a.err
#SBATCH --nodes=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=185G
#SBATCH -p reg
#SBATCH --mail-type=START,END,FAIL
#SBATCH --mail-user=bspencer@uidaho.edu,ryanmartin@uidaho.edu
#SBATCH --array=1-50   # <-- change range to however many tasks you want

# ------------------------------------------------
# Stagger Array Task Starts
# ------------------------------------------------
# Sleep for a little bit so nodes are intiallizing and trying to create temp files at same time
sleep $((SLURM_ARRAY_TASK_ID * 30))
# Your actual workload/command goes here
echo "Running task $SLURM_ARRAY_TASK_ID at $(date)"

# ------------------------------------------------
# Load modules
# ------------------------------------------------
set -euo pipefail

module purge

# ------------------------------------------------
# Activate Conda environment
# ------------------------------------------------
source ~/miniconda3/etc/profile.d/conda.sh
conda activate apptainer_env

# ------------------------------------------------
# Navigate to project directory
# ------------------------------------------------
cd ~/Teton_Microclim_Modeling

# ------------------------------------------------
# Create log folder
# ------------------------------------------------
mkdir -p logs/Endotherm_Model_v2

# ------------------------------------------------
# Create output directory for v2 test
# ------------------------------------------------
mkdir -p Endo_out_v2

# This host path is mounted in the container as /temp below. Do not use the
# host path itself for TMPDIR/TMP/TEMP inside the container.
export HOST_TMPDIR=/ceph/home/bspencer.ui/Teton_Microclim_Modeling/temp/job_${SLURM_JOB_ID}_array_${SLURM_ARRAY_TASK_ID}
mkdir -p "$HOST_TMPDIR"
mkdir -p "$HOST_TMPDIR/xdg_cache"

# Cell workspaces are removed by R after each chunk. Remove the remaining
# per-job Wine/R/Terra state after a normal job exit. On cancellation, retain
# it so the shell cannot delete inputs while Wine is still unwinding.
cleanup_tmp=1
trap 'cleanup_tmp=0; exit 143' TERM INT
trap 'if [ "$cleanup_tmp" -eq 1 ]; then rm -rf -- "$HOST_TMPDIR"; else echo "Cancelled: retaining $HOST_TMPDIR for safe cleanup."; fi' EXIT

echo "Host temporary directory: $HOST_TMPDIR"
df -h "$HOST_TMPDIR"
test -w "$HOST_TMPDIR"




# ------------------------------------------------
# Run R script with array ID
# ------------------------------------------------
echo "Running array task ${SLURM_ARRAY_TASK_ID}"

apptainer exec --no-mount home,cwd --pwd /temp \
  --env TMPDIR=/temp \
  --env TMP=/temp \
  --env TEMP=/temp \
  --env XDG_CACHE_HOME=/temp/xdg_cache \
  --env ENDO_WINEPREFIX=/temp/wine/prefix \
  --bind /ceph/home/bspencer.ui/Teton_Microclim_Modeling/R_code:/R_code \
  --bind /ceph/home/bspencer.ui/Teton_Microclim_Modeling/Microclim_out:/Microclim_out \
  --bind /ceph/home/bspencer.ui/Teton_Microclim_Modeling/Data:/Data \
  --bind /ceph/home/bspencer.ui/Teton_Microclim_Modeling/corrected_models:/corrected_models \
  --bind /ceph/home/bspencer.ui/Teton_Microclim_Modeling/NicheMapExe:/NicheMapExe \
  --bind /ceph/home/bspencer.ui/Teton_Microclim_Modeling/Endo_out_v2:/Endo_out_v2 \
  --bind "$HOST_TMPDIR":/temp \
  nichemapdockerenv.sif mamba run -n r_micro Rscript /R_code/Endotherm_LandscapeScale_v2.r \
    --clust_array_arg=$SLURM_ARRAY_TASK_ID \
    --clust_array_size=$SLURM_ARRAY_TASK_COUNT \
    --n_threads=$SLURM_CPUS_PER_TASK

echo "Task ${SLURM_ARRAY_TASK_ID} completed successfully."
