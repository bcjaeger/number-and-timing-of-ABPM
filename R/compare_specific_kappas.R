##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design_evaluated
##' @param winning_samplers
##' @param abpm_wide
##' @param boot_iters

compare_specific_kappas <- function(
  design_evaluated,
  id_1 = "3 BP measurements at 2, 3 and 4 hours after falling asleep",
  id_2 = "3 BP measurements at 1, 2 and 4 hours after falling asleep",
  abpm_wide,
  boot_iters = 1000
) {
  
  design_evaluated %>% 
    select(ID, study, time_var, strategy, n_msr, bp_means) %>% 
    filter(ID %in% c(id_1, id_2)) %>% 
    mutate(
      bp_means = map(
        .x = bp_means, 
        .f = left_join,
        select(abpm_wide, subjid, ends_with('full')), 
        by = 'subjid'
      )
    ) %>% 
    arrange(study, time_var, n_msr) %>% 
    split(.$study) %>% 
    map(~select(.x, ID, time_var, strategy, n_msr, bp_means)) %>% 
    #map(~mutate(.x, bp_means = map(bp_means, drop_na))) %>% 
    map(ID_decompose, remove_id = FALSE) %>% 
    map(arrange, strategy, strategy, n_msr) %>% 
    map_dfr(
      .id = 'study',
      .f = ~ {
        
        boot_data <- .x %>% 
          unnest(bp_means) %>% 
          select(ID, subjid, nht_full, nht_samp) %>% 
          pivot_wider(names_from = ID, values_from = nht_samp) %>% 
          select(all_of(c('subjid', 'nht_full', id_2, id_1))) %>% 
          set_names(c('subjid', 'nht_full', 'nht_samp_i', 'nht_samp_j'))
        
        boot_fun <- function(split, ...){
          analysis(split) %>% 
            summarize(
              kap_i = kap_ci(nht_full, nht_samp_i),
              kap_j = kap_ci(nht_full, nht_samp_j)
            ) %>% 
            unnest(cols = everything()) %>% 
            slice(1) %>% 
            transmute(
              term = 'Kappa difference',
              estimate = kap_i - kap_j
            )
        }
        
        
        boot_straps <- boot_data %>% 
          bootstraps(times = boot_iters, apparent = TRUE) %>% 
          mutate(results = map(splits, boot_fun)) 
        
        boot_pval <- boot_straps$results %>% 
          bind_rows(.id = 'boot') %>% 
          summarize(
            pval_lwr = 2 * mean(estimate < 0),
            pval_upr = 2 * mean(estimate > 0)
          ) %>% 
          mutate(pval = min(pval_lwr, pval_upr)) %>% 
          pull(pval)
        
        estimate <- boot_straps %>% 
          slice(boot_iters + 1) %>% 
          unnest(results) %>% 
          pull(estimate)
        
        boot_stats <- map_dbl(boot_straps$results, 'estimate')
        
        boot_interval <- bootBCa(estimate = estimate, estimates = boot_stats)
        
        tibble(
          lwr = boot_interval[1],
          est = estimate, 
          upr = boot_interval[2],
          pval = boot_pval
        )
        
      }
    )
  
}
