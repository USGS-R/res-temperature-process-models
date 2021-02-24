

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

res_io_obs <- arrow::read_feather('../delaware-model-prep/2_observations/tmp/res_io.feather')

# C exit codes: https://www.geeksforgeeks.org/exit-codes-in-c-c-with-examples/

glmtools::plot_temp(sprintf('tmp/sh/%s/output/output.nc', p1_reservoir_ids[1]))

png('cannonsville_ctr.png', width=1100, height=400, res=150, units='px')
glmtools::plot_temp(sprintf('tmp/ctr/%s/output/output.nc', p1_reservoir_ids[2]))
dev.off()


analyze_res <- function(res_id=p1_reservoir_ids[2]) {

  # read the extracted output files
  mac <- feather::read_feather(sprintf('3_run/out/mac/%s.feather', res_id))
  # ctr <- feather::read_feather(sprintf('3_run/out/ctr/%s.feather', res_id))
  # sh <- feather::read_feather(sprintf('3_run/out/sh/%s.feather', res_id))

  library(tidyverse)
  preds_long <- mac %>%
    select(-ice) %>%
    pivot_longer(names_to='temp_depth', values_to='wtemp', -time) %>%
    mutate(depth = as.numeric(gsub('temp_', '', temp_depth))) %>%
    select(-temp_depth)
  preds_long %>%
    filter(depth %in% c(0, 10, 40)) %>%
    mutate(Depth = as.factor(depth)) %>%
    ggplot(aes(x=time, y=wtemp, color=Depth)) +
    geom_line() +
    theme_classic()

  obs_rds <- '1_fetch/out/ltmp_temps.rds'
  obs_data <- read_rds(obs_rds) %>%
    filter(site_id == res_id) %>%
    select(datetime = date, depth, temp)
  obs_tsv <- sprintf('tmp/temps_%s.tsv', res_id)
  write_tsv(obs_data, obs_tsv)

  temp_matchups <- resample_to_field(
    sprintf('tmp/mac/%s/output/output.nc', res_id),
    obs_tsv,
    var_name = 'temp') %>%
    group_by(DateTime) %>%
    mutate(DepthRank = ordered(3 - length(Depth) + order(Depth), levels=1:3)) %>% # 1,2,3 when n=3; 2,3 otherwise (because the first depth category is the one that gets cut off when water levels drop)
    ungroup()
  temp_matchups %>%
    #filter(DepthRank == 1) %>%
    select(-Depth) %>%
    ggplot(aes(x=DateTime, color=DepthRank)) +
    geom_point(aes(y=Observed_temp)) +
    geom_line(aes(y=Modeled_temp), size=1) +
    scale_color_manual(values=RColorBrewer::brewer.pal(3, "Set2")) +
    theme_classic() +
    ylab('Temperature (deg C)') +
    ggtitle(sprintf('Preds (lines) and obs (points) for %s', res_id))
  ggsave(sprintf('4_inspect/out/predobsts_%s.png', res_id), width=5, height=5)

  # make plot of temperature predictions for a couple of years
  png(sprintf('4_inspect/out/preds2D_%s.png', res_id), width=1100, height=400, res=150, units='px')
  glmtools::plot_temp(sprintf('tmp/mac/%s/output/output.nc', res_id))
  dev.off()

  # return table of RMSEs
  temp_matchups %>%
    filter(!is.na(Modeled_temp), !is.na(Observed_temp)) %>%
    group_by(DepthRank) %>%
    summarize(
      n = length(Modeled_temp),
      MinDepth = min(Depth),
      MaxDepth = max(Depth),
      RMSE = sqrt(mean((Modeled_temp - Observed_temp)^2)))
}
analyze_res(res_id=p1_reservoir_ids[1])
# DepthRank     n MinDepth MaxDepth  RMSE
# <ord>     <int>    <dbl>    <dbl> <dbl>
# 1            23    0.128     2.02 0.593
# 2           110    8.39     13.9  3.55
# 3           110   19.7      25.2  1.32
analyze_res(res_id=p1_reservoir_ids[2])
# DepthRank     n MinDepth MaxDepth  RMSE
# <ord>     <int>    <dbl>    <dbl> <dbl>
# 2            26     7.47     10.9  1.78
# 3            31     6.33     27.3  1.74
