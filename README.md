# GLM modeling of reservoir temperatures

This repository is for running GLM models of reservoir temperatures in the Delaware River Basin, Upper Colorado River Basin, and beyond. I'm aiming to use Denali for all substantive model runs.

# Setup

## Quickstart

Here's what I usually run to get started each day. Details and explanations follow.
```sh
ssh denali.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/res-temperature-process-models

umask 002

sbatch launch-jlab.slurm
cat tmp/jlab.out
# copy-paste the ssh command into a new terminal window
# copy-paste the 127 URL into a new browser window
```

## Explanation

### Logging in

```sh
ssh denali.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/res-temperature-process-models
```

### Configuring session to share file access

By default on the USGS clusters, any time someone modifies a file, the file permissions change so that others are locked out. This made it really difficult to work collaboratively in one directory. To get around this, first give read-write access to everyone in your group for this whole directory (one time):
```sh
chmod g+w <filename or directory>
```

Then, for every session you start up, configure your session so that any changes you make to a file will not modify the file's permissions. Note that this is not something you want to do if you are working on files that only you should be able to change, e.g. your id_rsa keys.

```sh
umask 002
```

### Editing files on the cluster - Launching Jupyter Lab

Similar instructions are available for [tallgrass - lake temps](https://github.com/USGS-CIDA/lake-temperature-neural-networks/tree/master/2_model#editing-files-on-the-cluster), [yeti generally](https://hpcportal.cr.usgs.gov/hpc-user-docs/Yeti/Guides_and_Tutorials/how-to/Launch_Jupyter_Notebook.html), and [yeti - lake temps](https://github.com/USGS-R/lake-temperature-out/blob/master/README.md#editing-files-on-the-cluster---launching-jupyter-lab).

You can use `vim` to edit files locally.

You can also use the Jupyter interface to edit files via a browser-based IDE.

1. In a terminal window:
```sh
ssh denali.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/data-sci/lake-temp/res-temperature-process-models
sbatch launch-jlab.slurm
cat tmp/jlab.out
```

From the top of the output of the `cat` call, locate the `ssh` command between lines of `####`. Copy this command and paste it into a new terminal window (which will become permanently tied up).

From the last line of output of the `cat` call, copy the `http://127.0.0.1` URL and paste it into a local browser window. A jupyter lab session should open in your browser.

#### Preparing a software environment to run Jupyter and project code (once per user)

You may need to install miniconda3 in your home directory on caldera, or find some other way to access conda, before the following directions become possible.

To create a Jupyter-ready conda environment:
```sh
conda create -n jlab jupyterlab -c conda-forge
```

To add an R kernel to the Jupyter Lab IDE (so that we can build and run R notebooks in addition to Python notebooks):
```sh
conda activate jlab
conda install -c r r-irkernel zeromq
```
If you have already set up Jupyter Lab for the project (see below) and launched Jupyter Lab, you will have to re-launch Jupyter Lab (see above) to see the R kernel.

Lastly, we may not need the following for this project, but in other projects we've needed to install and point to project-specific R libraries. To do this, create and edit an .Renviron file within the project directory, to contain this line:
```sh
R_LIBS_USER="/caldera/projects/usgs/water/iidd/datasci/lake-temp/res-temperature-process-models/Rlib"
```

Installation of GitHub packages may go more smoothly if you run the installation from a login node (`ssh caldera-dtn.cr.usgs.gov`) rather than a Denali node.
