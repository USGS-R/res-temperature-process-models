# GLM modeling of reservoir temperatures

This repository is for running GLM models of reservoir temperatures in the Delaware River Basin, Upper Colorado River Basin, and beyond.

# Setup

## Quickstart

Here are the common commands. Details and explanations follow.
```sh
ssh -t denali.cr.usgs.gov 'cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/res-temperature-process-models && exec bash -l'
umask 002
```


## Log in

Run this repository on cluster nodes. I'm aiming to use Denali.
```sh
ssh denali.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/res-temperature-process-models
```

## Configure session to share file access

By default on the USGS clusters, any time someone modifies a file, the file permissions change so that others are locked out. This made it really difficult to work collaboratively in one directory. To get around this, first give read-write access to everyone in your group for this whole directory (one time):
```sh
chmod g+w <filename or directory>
```

Then, for every session you start up, configure your session so that any changes you make to a file will not modify the file's permissions. Note that this is not something you want to do if you are working on files that only you should be able to change, e.g. your id_rsa keys.

```sh
umask 002
```

## Editing files on the cluster - Launching Jupyter Lab
** These instructions are currently for Yeti and lake-temperature-out. Some changes will be needed. **

Similar instructions are available for [tallgrass](https://github.com/USGS-CIDA/lake-temperature-neural-networks/tree/master/2_model#editing-files-on-the-cluster) and [yeti](https://github.com/USGS-R/lake-temperature-out/blob/master/README.md#editing-files-on-the-cluster---launching-jupyter-lab).

You can use `vim` to edit files locally.

You can also use the Jupyter interface to edit files via a browser-based IDE. See https://hpcportal.cr.usgs.gov/hpc-user-docs/Yeti/Guides_and_Tutorials/how-to/Launch_Jupyter_Notebook.html for more.

Once you have set up a script to launch Jupyter Lab for the project and created the jlab environment for the user (see instructions below), follow these steps:

1. In a new terminal window (call this one Terminal #2, assuming you'll keep one open for terminal access to Yeti):
```sh
ssh yeti.cr.usgs.gov
cd /cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-out
module load legacy
module load python/anaconda3
salloc -J jlab -t 2:00:00 -p normal -A iidd -n 1 -c 1
sh launch-jlab.sh
```
and copy the first line printed out by that script (begins with `ssh`). Note that this terminal is now tied up.

2. In another new terminal window (call this one Terminal #3), paste the ssh command, which will look something like this:
```sh
 ssh -N -L 8599:igskahcmgslih03.cr.usgs.gov:8599 hcorson-dosch@yeti.cr.usgs.gov
```
Enter the command. Note that this terminal is now tied up.

#### Creating a conda Jupyter Lab environment (once per user)
```sh
module load legacy
module load python/anaconda3
conda create -n jlab jupyterlab -c conda-forge
```

In order to add an R kernel to the Jupyter Lab IDE (so that we can build and run R notebooks in addition to Python notebooks), we need to run the following series of commands:
```sh
module load legacy
module load python/anaconda3
conda activate jlab
conda install -c r r-irkernel zeromq
```
If you have already set up Jupyter Lab for the project (see below) and launched Jupyter Lab, you will have to re-launch Jupyter Lab (see above) to see the R kernel.

#### Creating a script to launch Jupyter Lab (once per project)
Save the following script to `launch-jlab.sh`.

```sh
#!/bin/bash

JPORT=`shuf -i 8400-9400 -n 1` 

source activate jlab

echo "ssh -N -L $JPORT:`hostname`:$JPORT $USER@yeti.cr.usgs.gov"

jupyter lab --ip '*' --no-browser --port $JPORT --notebook-dir=. &

wait
```

Next we need to add the base R library from the Yeti R 3.6.3 module to our .Renviron file, so that it can be accessed by the Rkernel in Jupyter Lab.

In the console, within the project directory, type `vim .Renviron`. Enter 'i' to enter the insert mode, and paste in the following line:

```sh
R_LIBS_USER="/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-out/Rlib_3_6":"/opt/ohpc/pub/usgs/libs/gnu8/R/3.6.3/lib64/R/library"
```
Press 'Esc', then type ':wq' to save and close the file.

Launch Jupyter Lab (see above) and open a new Jupyter Notebook with the R kernel. Run the command `.libPaths()`. You should see these 3 paths listed in this order:
```sh
'/cxfs/projects/usgs/water/iidd/data-sci/lake-temp/lake-temperature-out/Rlib_3_6'
'/opt/ohpc/pub/usgs/libs/gnu8/R/3.6.3/lib64/R/library'
'/home/{username}/.conda/envs/jlab/lib/R/library'
```
Now we should be able to load any libraries from our project library folder while in Jupyter Lab, and any necessary dependencies that are not in our project library folder will be loaded from the Yeti R 3.6.3 module library.
