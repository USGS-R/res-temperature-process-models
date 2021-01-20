# GLM modeling of reservoir temperatures

This repository is for eventually running GLM models of reservoir temperatures in the Delaware River Basin, Upper Colorado River Basin, Missouri, and beyond. For now it's just the Pepacton (`nhdhr_151957878`) and Cannonsville (`nhdhr_120022743`) reservoirs in the DRB. I'm aiming to use Denali for all substantive model runs.

# Quickstart

Here's what I usually run at the start of a work session.
```sh
ssh denali.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/res-temperature-process-models

umask 002

sbatch launch-jlab.slurm
cat tmp/jlab.out
# copy-paste the ssh command into a new terminal window
# copy-paste the 127 URL into a new browser window
```

See "Explanation of Quickstart" below for details.

# Shifter

For any of the following applications, you'll need the shifter module:
```sh
module load shifter
```

Here's how to get the image from Dockerhub and translate it to Shifter. This took about 3 minutes on Denali (and can't be done on the data transfer node because there's no shifter module there, I think):
```sh
shifterimg pull docker:aapplingusgs/glm3r:v0.4
```
I created my own image because David's glm3r:v0.3 installed glm into /home/rstudio/..., which was getting masked by the caldera /home directories when I ran the shifter container. I think this behavior differs from Docker and Singularity, which don't make host directories available on the container unless you specifically map them - or maybe Singularity does some of this, but the rules are different.

Here's how to run a script such as tmp/glm_test.R using the shifter container:
```sh
shifter --image=docker:aapplingusgs/glm3r:v0.4 Rscript -e 'source("tmp/test_glm3.R")'
```
It even says "Model Run Complete"!

Here's how to run the shifter container interactively on the login node (do this rarely):
```sh
shifter --image=docker:aapplingusgs/glm3r:v0.4 /bin/bash
# when you're in the shifter environment, the prompt starts with "I have no name!@"
conda deactivate # if you don't do this, the miniconda R will override the shifter R
R
library(GLM3r)
# etc
q()
exit
```

Here's how to run the shifter container interactively on an allocated job:
```sh
salloc -c 1 --image=docker:aapplingusgs/glm3r:v0.4 -t 00:30:00 -A watertemp shifter /bin/bash
# when you're in the shifter environment, the prompt starts with "I have no name!@"
conda deactivate # if you don't do this, the miniconda R will override the shifter R
R
library(GLM3r)
# etc
q()
exit
```

It should even be possible to run the Jupyter Lab with a shifter kernel, but I'm going to leave this for another day: https://docs.nersc.gov/services/jupyter/#shifter-kernels-on-jupyter. In the meantime, the interactive shifter container code given above will work on a Terminal within Jupyter Lab (or a regular Terminal, of course).

# Explanation of Quickstart

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

A local Rlibs folder and an .Renviron file that points to it are included in the git repo for this project. If it's needed, installation of GitHub packages may go more smoothly if you run the installation from a login node (`ssh caldera-dtn.cr.usgs.gov`) rather than a Denali node.
