

##' @title
##' @param truth 
##' @param estimate 
kap_ci <- function(truth, estimate){
  
  kappa <- fmsb::Kappa.test(x=truth, y=estimate)$Result
  list(
    est = kappa$estimate,
    lwr = kappa$conf.int[1],
    upr = kappa$conf.int[2]
  )
  
}


##' @title
##' @param x
boot_sample <- function(x){
  x[sample(1:length(x), replace=TRUE)]
}

##' @title
##' @param x
##' @param nboots
boot_ci <- function(x, nboots = 1000){
  
  boot_results <- rep(NA_real_, nboots)
  
  for(i in seq(nboots)){
    boot_results[i] <- mean(boot_sample(x))
  }
  
  list(
    est = quantile(boot_results, 0.500, names = FALSE),
    lwr = quantile(boot_results, 0.025, names = FALSE),
    upr = quantile(boot_results, 0.975, names = FALSE)
  )
  
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design_init
evaluate_bp_samplers <- function(design_init, abpm_wide, boot_iters) {

  .abpm_wide <- abpm_wide %>% 
    select(subjid, slp_sbp_full, slp_dbp_full, nht_full)
  
  kap_acc <- design_init %>% 
    unnest(bp_means) %>% 
    left_join(.abpm_wide, by = 'subjid') %>% 
    mutate(
      nht_full = factor(nht_full, levels = c('no', 'yes')),
      nht_samp = factor(nht_samp, levels = c('no', 'yes'))
    ) %>% 
    group_by(ID, study) %>% 
    summarise(
      kap_nht = list(kap_ci(truth = nht_full, estimate = nht_samp)),
      mae_sbp = list(boot_ci(x = abs(slp_sbp_full-slp_sbp_samp), boot_iters)),
      mae_dbp = list(boot_ci(x = abs(slp_dbp_full-slp_dbp_samp), boot_iters)),
      .groups = 'drop'
    ) %>%
    #mutate(abp_id = as.character(abp_id)) %>% 
    unnest_wider(col = kap_nht, names_sep = '_') %>% 
    unnest_wider(col = mae_sbp, names_sep = '_') %>% 
    unnest_wider(col = mae_dbp, names_sep = '_')
  
  left_join(design_init, kap_acc, by = c('ID', 'study'))

}
