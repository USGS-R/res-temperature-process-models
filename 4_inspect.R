

# p4 <- list(
#   tar_target(
#     p4_temp_plot,
#     glmtools::plot_temp(sprintf('.sim-scratch/%s/output/output.nc', p1_reservoir_ids)),
#     pattern = map(p1_reservoir_ids))
# )

library(targets)
library(glmtools)
tar_load(p1_reservoir_ids)

GLM3r::run_glm(sprintf('.sim-scratch/%s', p1_reservoir_ids[1]), verbose = FALSE)
GLM3r::run_glm(sprintf('.sim-scratch/%s', p1_reservoir_ids[2]), verbose = FALSE)

sglmtools::plot_temp(sprintf('.sim-scratch/%s/output/output.nc', p1_reservoir_ids[1]))
glmtools::plot_temp(sprintf('.sim-scratch/%s/output/output.nc', p1_reservoir_ids[2]))
