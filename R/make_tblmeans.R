#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param design_evaluated
make_tblmeans <- function(design_evaluated,
                          abpm_wide) {

  full_abpm_bystudy <- abpm_wide %>% 
    group_by(study) %>% 
    summarize(
      across(
        .cols = c(slp_sbp_full, slp_dbp_full),
        .fns = list(m = mean, s = sd)
      )
    ) %>% 
    mutate(study = as.character(study))
  
  full_abpm_overall <- abpm_wide %>% 
    summarize(
      across(
        .cols = c(slp_sbp_full, slp_dbp_full),
        .fns = list(m = mean, s = sd)
      )
    ) %>% 
    mutate(study = 'Overall')
  
  full_abpm <- bind_rows(full_abpm_overall,
                         full_abpm_bystudy) %>% 
    mutate(n_msr = "Full night of ABPM") %>% 
    rename(sbp_mean = slp_sbp_full_m,
           sbp_sd = slp_sbp_full_s,
           dbp_mean = slp_dbp_full_m,
           dbp_sd = slp_dbp_full_s)

  design_evaluated %>% 
    select(ID, study, time_var, strategy, n_msr, bp_means) %>% 
    mutate(
      bp_mean_values = map(
        .x = bp_means,
        .f = ~.x %>% 
          summarize(across(where(is.numeric), list(m=mean, s=sd)))
      )
    ) %>% 
    unnest_wider(bp_mean_values) %>% 
    select(
      ID, 
      study,
      time_var, 
      strategy,
      n_msr, 
      sbp_mean = slp_sbp_samp_m,
      sbp_sd = slp_sbp_samp_s,
      dbp_mean = slp_dbp_samp_m,
      dbp_sd = slp_dbp_samp_s
    ) %>% 
    bind_rows(full_abpm)

}
