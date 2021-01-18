#!/bin/bash
#SBATCH -J jlab
#SBATCH -t 4:00:000
#SBATCH -o tmp/jlab.out
#SBATCH -A watertemp
#SBATCH -n 1
#SBATCH -c 1

JPORT=`shuf -i 8400-9400 -n 1`

# load conda and conda environment
# conda environment should have been created prior to submitting job, e.g.
# conda create -n jlab jupyterlab -c conda-forge
source activate jlab

echo
echo "##################################################################"
echo "Run the following ssh command from a new terminal on your desktop:"
echo
echo "ssh -N -L $JPORT:`hostname`:$JPORT $USER@denali.cr.usgs.gov"
echo
echo "Then copy and paste the 127.0.0.1 URL below into a browser on your desktop"
echo "##################################################################"
echo

# launch the jupyter jobo
srun jupyter lab --ip '*' --no-browser --port $JPORT --notebook-dir .

