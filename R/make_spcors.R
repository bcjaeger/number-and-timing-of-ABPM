##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param study_design
make_spcors <- function(study_design) {

  pre_ranked <- study_design %>% 
    select(ID, study, time_var, n_msr, ends_with('est')) %>% 
    pivot_wider(values_from = ends_with('est'), names_from = study) 
  
  suppressWarnings(list(
    kappa = pre_ranked %$% 
      cor.test(kap_nht_est_CARDIA, kap_nht_est_JHS, method = 'spearman') %>% 
      use_series('estimate'),
    mae_sbp = pre_ranked %$% 
      cor.test(mae_sbp_est_CARDIA, mae_sbp_est_JHS, method = 'spearman') %>% 
      use_series('estimate'),
    mae_dbp = pre_ranked %$% 
      cor.test(mae_dbp_est_CARDIA, mae_dbp_est_JHS, method = 'spearman') %>% 
      use_series('estimate')
  )) %>% 
    map(set_names, NULL)
    
}
