##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design_evaluated
##' @param winning_samplers
##' @param abpm_wide
##' @param boot_iters

compare_kappas <- function(
  design_evaluated,
  winning_samplers,
  abpm_wide,
  boot_iters
) {
  
  kappa_compare <- design_evaluated %>% 
    select(ID, study, time_var, strategy, n_msr, bp_means) %>% 
    filter(ID %in% winning_samplers) %>% 
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
    map(arrange, strategy, strategy, n_msr)
  
  results <- as_tibble(expand.grid(i = 1:12, j = 1:12)) %>% 
    mutate(
      study = case_when(i < j ~ 'CARDIA', i > j ~ 'JHS'),
      lab_i = factor(i, labels = kappa_compare$CARDIA$ID),
      lab_j = factor(j, labels = kappa_compare$CARDIA$ID),
      .results = list(NULL)
    )
  
  for (k in seq(nrow(results))) {
    
    i = results$i[k]
    j = results$j[k]
    
    msg <- paste(' iteration ', k, ': i = ', i, ', j = ', j, sep = '')
    print(msg)
    
    if( i == j ){
      
      # present kappa stats
      .results <- list(
        CARDIA = kappa_compare$CARDIA$bp_means[[i]],
        JHS = kappa_compare$JHS$bp_means[[j]]
      ) %>% 
        map_dfr(
          ~ .x %$% 
            kap_ci(estimate = nht_samp, truth = nht_full) %>% 
            as_tibble() %>% 
            mutate(pval = 1),
          .id = 'study'
        ) 
      
    } else {
      
      # compare kappas in JHS or CARDIA
      if ( i < j ) .study <- 'CARDIA' else .study <- 'JHS'
      
      sampler_i = kappa_compare[[.study]][['bp_means']][[i]] %>% 
        select(subjid, nht_full, nht_samp_i = nht_samp)
      
      sampler_j = kappa_compare[[.study]][['bp_means']][[j]] %>% 
        select(subjid, nht_samp_j = nht_samp)
      
      boot_data <- left_join(sampler_i, sampler_j, by = 'subjid')
      
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
      
      .results <- tibble(
        lwr = boot_interval[1],
        est = estimate, 
        upr = boot_interval[2],
        pval = boot_pval
      )
      
    }
    
    results$.results[[k]] <- .results
    
  }
  
  results

}
