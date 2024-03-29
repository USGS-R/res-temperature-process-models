# GLM modeling of reservoir temperatures

This repository is for eventually running GLM models of reservoir temperatures in the Delaware River Basin, Upper Colorado River Basin, Missouri, and beyond. For now it's just the Pepacton (`nhdhr_151957878`) and Cannonsville (`nhdhr_120022743`) reservoirs in the DRB. I'm aiming to use Denali for all substantive model runs.

# Quickstart

Here's what I usually run at the start of a work session.
```sh
ssh denali.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/res-temperature-process-models

umask 002

sbatch launch-jlab.slurm # -t 08:00:00
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
shifterimg pull docker:aapplingusgs/glm3r:v0.5
```
I created my own image because:
1. David's glm3r:v0.3 installed glm into /home/rstudio/..., which was getting masked by the caldera /home directories when I ran the shifter container. I think this behavior differs from Docker and Singularity, which don't make host directories available on the container unless you specifically map them - or maybe Singularity does some of this, but the rules are different.
2. I wanted to have `targets`, `glmtools`, and other packages installed so I could build the models as targets. It's easier to install these packages while building a Docker image than while on Denali - I keep hitting 404s and other https issues when trying to `install_github` on Denali.


Here's how to build targets using the shifter container:
```sh
salloc -c 1 --image=docker:aapplingusgs/glm3r:v0.5 -t 00:30:00 -A watertemp shifter Rscript -e 'targets::tar_make(p3_glm_sh_out)'
```

Here's how to run the shifter container interactively on an allocated job:
```sh
salloc -c 1 --image=docker:aapplingusgs/glm3r:v0.5 -t 00:30:00 -A watertemp shifter /bin/bash
# when you're in the shifter environment, the prompt starts with "I have no name!@"
conda deactivate # if you don't do this, the miniconda R will override the shifter R
R
library(GLM3r)
# etc
q()
exit
```


## Other shifter options

Here are some examples running shifter on the login node (do this rarely):

Run a script such as tmp/glm_test.R:
```sh
shifter --image=docker:aapplingusgs/glm3r:v0.5 Rscript -e 'source("tmp/test_glm3.R")'
```

Get a bash environment for interactive work on the shifter container:
```sh
shifter --image=docker:aapplingusgs/glm3r:v0.5 /bin/bash
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

A local Rlibs folder and an .Renviron file that points to it are included in the git repo for this project. If it's needed, installation of GitHub packages may go more smoothly if you run the installation from a Caldera node (`ssh caldera-dtn.cr.usgs.gov`) rather than a Denali Slurm-allocated node.

### Using the GLM container locally (with Docker)

Recall that the goal is to use Denali+Shifter rather than local+Docker, but if you must:

1. Git clone https://github.com/wdwatkins/glm3r_docker

2. Launch Docker Desktop.

From the glm3r_docker working directory:
```sh
docker-compose up
```

### Installing GLM locally

The Shifter/Docker container should have reproducibility benefits but adds layers of complexity and may sometimes be slower (at least for Docker). To install run GLM3 on a local computer:

1. Install home-brew (https://brew.sh/)
```sh
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

2. Open README.Macintosh at https://github.com/AquaticEcoDynamics/GLM and follow the instructions
    1. Install Xcode (and command line): `xcode-select —install`
    2. Get fortran: `brew install gcc`
    3. Get NetCDF: `brew install netcdf`
    4. Get libgd: `brew install gd`
    5. Get make: `brew install cmake`
    
3. Clone all repositories on local machine
```sh
mkdir AquaticEcoDynamics
cd AquaticEcoDynamics
git clone https://github.com/AquaticEcoDynamics/libplot.git
git clone https://github.com/AquaticEcoDynamics/libaed2.git
git clone https://github.com/AquaticEcoDynamics/libutil.git
git clone https://github.com/AquaticEcoDynamics/GLM.git
git clone https://github.com/AquaticEcoDynamics/libaed-water.git
```

4. Go into GLM/, checkout your version of choice, and modify GLM_CONFIG
```sh
cd GLM
git checkout tags/v3.1.0 -b v3.1.0 # or do "git clone -b v3.1.0 https://github.com/AquaticEcoDynamics/GLM.git" above
vim GLM_CONFIG
```
In `vim`, remove `#` before `export FC=gfortran` and `#` before `export HOMEBREW=true`
OR: `export FC=ifort`

5. Run `./build_glm.sh`

6. Install GLM3r and point it to the newly built glm executable
```
remotes::install_github("jsta/GLM3r")
Sys.setenv(GLM_PATH = "/path/to/AquaticEcoDynamics/GLM/glm")
```
once you've tested it, put that `Sys.setenv()` call in an .Rprofile file in this project's directory, or make the corresponding edit to a local .Renviron file.

