##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param study_analyzed
##' @param winning_samplers
make_figerrs <- function(study_design, abpm_wide, winning_samplers) {

  winning_ids <- winning_samplers$abp_id
  
  gg_dat <- study_design %>% 
    select(abp_id:bp_means, -sample_times) %>% 
    unnest(col = bp_means) %>% 
    left_join(select(abpm_wide, subjid, ends_with('full'))) %>% 
    group_by(abp_id) %>% 
    nest() %>% 
    mutate(
      mdl = map(
        .x = data, 
        .f = ~ lm(slp_sbp_samp - slp_sbp_full ~ slp_sbp_full, data = .x)
      ),
      mdl_tidy = map(mdl, tidy, conf.int = TRUE),
      mdl_pv = map_dbl(mdl, 
        .f = ~{
          sm = summary(.x)
          1 - pf(
            sm$fstatistic['value'], 
            sm$fstatistic['numdf'], 
            sm$fstatistic['dendf']
          )
        },
      )
    ) %>% 
    filter(abp_id %in% winning_ids)
  
  slope_dat <- gg_dat %>% 
    group_by(n_msr, time_var) %>% 
    summarize(
      slope = lm_slope(slp_sbp_samp - slp_sbp_full ~ slp_sbp_full),
      intercept = lm_intercept(slp_sbp_samp - slp_sbp_full ~ slp_sbp_full)
    ) %>% 
    right_join(gg_dat, by = 'abp_id') %>% 
    mutate(abp_soft_id = glue(
      "{str_replace(n_msr, 'msr', ' times')} {sample_label}"
    ))
  
  ggplot(slope_dat) + 
    aes(x = slp_sbp_full, y = slp_sbp_samp - slp_sbp_full) + 
    geom_smooth(aes(col = abp_id), se = FALSE, method = 'lm') +
    gghighlight(abs(slope) < 0.1 & abs(intercept) < 0.1) +
    facet_grid(time_var~study)

}
