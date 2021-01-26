library(targets)
library(tarchetypes)
library(tidyverse)
tar_option_set(packages = 'tidyverse')
source('lib/src/hash_files.R')

source('1_fetch.R')
source('2_prep.R')

# Return the complete list of targets
c(p1, p2)
