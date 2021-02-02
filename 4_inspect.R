

# p4 <- list(
#   tar_target(
#     p4_temp_plot,
#     glmtools::plot_temp(sprintf('.sim-scratch/%s/output/output.nc', p1_reservoir_ids)),
#     pattern = map(p1_reservoir_ids))
# )

library(targets)
library(glmtools)
tar_load(p1_reservoir_ids)

dir.create('tmp/mac', recursive=TRUE)
GLM3r::run_glm(sprintf('tmp/mac/%s', p1_reservoir_ids[1]), verbose = FALSE)
GLM3r::run_glm(sprintf('tmp/mac/%s', p1_reservoir_ids[2]), verbose = FALSE)

dir.create('tmp/ctr', recursive=TRUE)
GLM3r::run_glm(sprintf('tmp/ctr/%s', p1_reservoir_ids[1]), verbose = FALSE)
GLM3r::run_glm(sprintf('tmp/ctr/%s', p1_reservoir_ids[2]), verbose = FALSE)

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
