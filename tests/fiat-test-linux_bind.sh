#!/bin/bash
#SBATCH -N2
#SBATCH --time 00:05:00

set -x

module load intel > /dev/null 2>&1
module load intelmpi > /dev/null 2>&1

NNODE=$SLURM_NNODES
NTASK=32
NOPMP=4
let "NPROC=$NNODE*$NTASK"

$SLURM_SUBMIT_DIR/fiat-test-linux_bind.pl nodes $SLURM_NNODES tasks $NTASK openmp $NOPMP

export OMP_NUM_THREADS=$NOPMP

# Works (linux_bind1_)
srun --ntasks $NPROC $SLURM_SUBMIT_DIR/fiat-test-linux_bind 1

# Fails (linux_bind_)
srun --ntasks $NPROC $SLURM_SUBMIT_DIR/fiat-test-linux_bind 0

