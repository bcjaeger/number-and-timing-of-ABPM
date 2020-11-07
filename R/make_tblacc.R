##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param winning_samplers
##' @param design_evaluated
make_tblacc <- function(winning_samplers, design_evaluated) {
  
  rspec <- round_spec() %>% 
    round_half_even() %>% 
    round_using_magnitude(breaks = c(1, 10, 100, Inf),
                          digits = c(2, 2, 1, 0))
  
  list(
    winners = filter(design_evaluated, ID %in% winning_samplers),
    everyone = design_evaluated
  ) %>%
    map(
      ~ .x %>% 
        arrange(n_msr) %>% 
        mutate(
          tbv_kap = table_glue(
            "{kap_nht_est}\n({kap_nht_lwr}, {kap_nht_upr})", rspec = rspec
          ),
          tbv_sbp = table_glue(
            "{mae_sbp_est}\n({mae_sbp_lwr}, {mae_sbp_upr})", rspec = rspec
          ),
          tbv_dbp = table_glue(
            "{mae_dbp_est}\n({mae_dbp_lwr}, {mae_dbp_upr})", rspec = rspec
          ),
          n_msr = if_else(
            condition = strategy == 'cnctr',
            true = str_replace(n_msr, 'msr', ' Consecutive BP measurements'),
            false = str_replace(n_msr, 'msr', ' Distributed BP measurements')
          )
        ) %>%
        select(ID, strategy, study, starts_with('tbv')) %>% 
        pivot_wider(names_from = study, values_from = starts_with('tbv')) %>%
        select(ID, strategy, 
          tbv_kap_Overall,
          tbv_kap_CARDIA,
          tbv_kap_JHS,
          tbv_sbp_Overall,
          tbv_sbp_CARDIA,
          tbv_sbp_JHS,
          tbv_dbp_Overall,
          tbv_dbp_CARDIA,
          tbv_dbp_JHS) %>% 
        ID_decompose()
    )
  
  
}
