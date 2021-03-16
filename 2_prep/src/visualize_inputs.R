#### Input Data ####

# Plot outflow flow predictions and observations. By this point all flows should be in cms
plot_outflows <- function(inouts, outflows, res_id, out_dir) {
  # Munge
  all_inouts <- bind_rows(
    inouts %>% filter(location == 'inflow') %>% select(location, id=seg_id_nat, time, flow),
    outflows %>% mutate(location = 'outflow') %>% select(location, id=release_type, time, flow)) %>%
    mutate(year = lubridate::year(time)) %>%
    group_by(location, year, id) %>%
    summarize(flow_by_reach = mean(flow), .groups='drop') %>%
    mutate(flow_by_reach = case_when(location == 'inflow' ~ flow_by_reach, TRUE ~ -flow_by_reach)) %>%
    group_by(year) %>%
    mutate(net_flow = sum(flow_by_reach))

  # Plot
  all_inouts %>%
    ggplot(aes(fill=id, x=year, y=flow_by_reach, group=location)) +
    geom_bar(stat = 'identity', position = position_stack()) +
    geom_step(aes(y=net_flow), direction = 'mid') +
    scale_color_discrete('Flowpath ID') +
    xlab('Year') +
    ylab('Annual mean flow rate (cms)') +
    theme_bw() +
    ggtitle(sprintf('Raw water budget for %s (%s)', names(res_id), res_id))

  # Save the plot
  out_file <- file.path(out_dir, sprintf('water_budget_raw_%s.png', res_id))
  ggsave(out_file, width=6, height=4)
  return(out_file)
}


# Data exploration (originally in build_nml_edits)
# pep_out <- inout_feather %>% arrow::read_feather() %>% filter(res_id == 'nhdhr_151957878', location == 'outflow')
# can_out <- inout_feather %>% arrow::read_feather() %>% filter(res_id == 'nhdhr_120022743', location == 'outflow')
# pep_rel <- releases_csv %>% readr::read_csv(col_types=cols()) %>% filter(reservoir == 'Pepacton') %>% mutate(flow=release_volume_cfs*0.028316847)
# can_rel <- releases_csv %>% readr::read_csv(col_types=cols()) %>% filter(reservoir == 'Cannonsville') %>% mutate(flow=release_volume_cfs*0.028316847)
# pep_rel_tot <- pep_rel %>%
#   group_by(date) %>%
#   summarize(flow = sum(flow), .groups='drop')
# can_rel_tot <- can_rel %>%
#   group_by(date) %>%
#   summarize(flow = sum(flow), .groups='drop')
# # Pepacton
# pep_rel %>% group_by(release_type) %>% summarize(sum(flow))
# # release_type `sum(flow)`
# # Conser            44956.
# # Direct            12480.
# # Spill             48538.
# ggplot(pep_rel, aes(x=date, y=flow, color=release_type)) + geom_line()
# pep_matchups <- select(pep_out, date, outflow_prms=seg_outflow) %>% inner_join(select(pep_rel_tot, date, outflow_releases=flow), by='date')
# pep_matchups %>% summarize(sum(outflow_prms), sum(outflow_releases))
# # `sum(outflow_prms)` `sum(outflow_releases)`
# #             222619.                 105974.    # big mismatch
# pep_matchups %>% ggplot(aes(x=outflow_prms, y=outflow_releases)) + geom_abline() + geom_point(alpha=0.5) + theme_bw() # plenty of daily mismatches
# # Cannonsville
# can_rel %>% group_by(release_type) %>% summarize(sum(flow))
# # release_type `sum(flow)`
# # Conser            88273.
# # Direct            58281.
# # Spill            101432.
# ggplot(can_rel, aes(x=date, y=flow, color=release_type)) + geom_line()
# can_matchups <- select(can_out, date, outflow_prms=seg_outflow) %>% inner_join(select(can_rel_tot, date, outflow_releases=flow), by='date')
# can_matchups %>% summarize(sum(outflow_prms), sum(outflow_releases))
# # `sum(outflow_prms)` `sum(outflow_releases)`
# #             254779.                 247986.    # much closer than pepacton
# can_matchups %>% ggplot(aes(x=outflow_prms, y=outflow_releases)) + geom_abline() + geom_point(alpha=0.5) + theme_bw() # still plenty of daily mismatches
