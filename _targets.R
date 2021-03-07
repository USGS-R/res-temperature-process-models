library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(tidyverse))
tar_option_set(packages = 'tidyverse', error = 'continue')

source('1_fetch.R')
source('2_prep.R')
source('3_run.R')

# Return the complete list of targets
c(p1, p2, p3)
