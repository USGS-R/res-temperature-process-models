

# p4 <- list(
#   tar_target(
#     p4_temp_plot,
#     glmtools::plot_temp(sprintf('.sim-scratch/%s/output/output.nc', p1_reservoir_ids)),
#     pattern = map(p1_reservoir_ids))
# )

library(targets)
library(glmtools)
library(tidyverse)
tar_load(c('p1_reservoir_ids','p2_meteo','p2_nml_objects'))

# C exit codes: https://www.geeksforgeeks.org/exit-codes-in-c-c-with-examples/

system('diff tmp/mac/nhdhr_151957878/glm3.nml tmp/mac/nhdhr_120022743/glm3.nml')
# differences are in:
diff_vars <- c(
  'latitude',
  'longitude',
  'bsn_len',
  'bsn_wid',
  'H',
  'A',
  'lake_depth',
  'the_depths',
  'meteo_fl',
  'cd',
  'Kw')

# original runs
GLM3r::run_glm(sprintf('tmp/mac/%s', p1_reservoir_ids[1]), verbose = FALSE) # exit code 139
GLM3r::run_glm(sprintf('tmp/mac/%s', p1_reservoir_ids[1]), verbose = TRUE) # exit code 11
GLM3r::run_glm(sprintf('tmp/mac/%s', p1_reservoir_ids[2]), verbose = FALSE) # exit code 0 (success)

# reproduce in a test dir
nml_orig <- p2_nml_objects[[1]]
nml_swap <- p2_nml_objects[[2]]
meteo_fl_orig <- glmtools::get_nml_value(nml_orig, 'meteo_fl')
meteo_fl_swap <- glmtools::get_nml_value(nml_swap, 'meteo_fl')
meteo_orig <- p2_meteo[[1]]
meteo_swap <- p2_meteo[[2]]

# reproduce segfault/exit(11)
test_dir <- 'tmp/mac/test0'; dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(p2_meteo[[1]], file.path(test_dir, meteo_fl_orig))
glmtools::write_nml(nml_orig, file.path(test_dir, 'glm3.nml'))
GLM3r::run_glm(test_dir, verbose = TRUE)
# code 11 when verbose=TRUE, 139 otherwise

# try using the other site's meteo file
test_dir <- 'tmp/mac/test1'; dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(meteo_swap, file.path(test_dir, meteo_fl_swap))
nml_orig %>%
  glmtools::set_nml('meteo_fl', meteo_fl_swap) %>%
  glmtools::write_nml(file.path(test_dir, 'glm3.nml'))
GLM3r::run_glm(test_dir, verbose = TRUE)
# code 11 when verbose, 139 otherwise

# borrow H and A
test_dir <- 'tmp/mac/test2'; dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(meteo_orig, file.path(test_dir, meteo_fl_orig))
p2_nml_objects[[1]] %>%
  glmtools::set_nml('H', glmtools::get_nml_value(nml_swap, 'H')) %>%
  glmtools::set_nml('A', glmtools::get_nml_value(nml_swap, 'A')) %>%
  glmtools::write_nml(file.path(test_dir, 'glm3.nml'))
GLM3r::run_glm(test_dir, verbose = TRUE)
# code 0!

HA_orig <- tibble(H = glmtools::get_nml_value(nml_orig, 'H'), A = glmtools::get_nml_value(nml_orig, 'A'))
HA_swap <- tibble(H = glmtools::get_nml_value(nml_swap, 'H'), A = glmtools::get_nml_value(nml_swap, 'A'))
ggplot(HA_orig, aes(x=A, y=H)) +
  geom_line(color='orange') + # orange, orig, get it?
  geom_line(data=HA_swap, color='blue') # out of the blue?

# should crest_elev be changing?
test_dir <- 'tmp/mac/test3'; dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(meteo_orig, file.path(test_dir, meteo_fl_orig))
p2_nml_objects[[1]] %>%
  glmtools::set_nml('crest_elev', 350) %>%
  glmtools::write_nml(file.path(test_dir, 'glm3.nml'))
GLM3r::run_glm(test_dir, verbose = TRUE)

# other experiments

dir.create('tmp/ctr', recursive=TRUE)
GLM3r::run_glm(sprintf('tmp/ctr/%s', p1_reservoir_ids[1]), verbose = FALSE)
GLM3r::run_glm(sprintf('tmp/ctr/%s', p1_reservoir_ids[2]), verbose = FALSE)


glmtools::plot_temp(sprintf('tmp/mac/%s/output/output.nc', p1_reservoir_ids[1]))

png('cannonsville_mac.png', width=1100, height=400, res=150, units='px')
glmtools::plot_temp(sprintf('tmp/mac/%s/output/output.nc', p1_reservoir_ids[2]))
dev.off()
png('cannonsville_ctr.png', width=1100, height=400, res=150, units='px')
glmtools::plot_temp(sprintf('tmp/ctr/%s/output/output.nc', p1_reservoir_ids[2]))
dev.off()

mac <- feather::read_feather('3_run/out/mac/nhdhr_120022743.feather')
ctr <- feather::read_feather('3_run/out/ctr/nhdhr_120022743.feather')

library(tidyverse)
mac_df <- mac %>%
  pivot_longer(names_to='temp_depth', values_to='wtemp', -time) %>%
  mutate(depth = as.numeric(gsub('temp_', '', temp_depth))) %>%
  select(-temp_depth)
mac_df %>%
  filter(depth %in% c(0, 10, 40)) %>%
  mutate(Depth = as.factor(depth)) %>%
  ggplot(aes(x=time, y=wtemp, color=Depth)) +
  geom_line() +
  theme_classic()
