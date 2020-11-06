##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param winning_samplers
##' @param design_evaluated
make_tblacc <- function(winning_samplers, design_evaluated) {

  tbl_data <- list(
    winners = filter(design_evaluated, ID %in% winning_samplers),
    everyone = design_evaluated
  ) %>%
    map(
      ~ .x %>% 
        arrange(n_msr) %>% 
        mutate(
          tbv_kap = tbl_string(
            "{kap_nht_est}\n({kap_nht_lwr}, {kap_nht_upr})",
            decimals = c(2, 2, 1)
          ),
          tbv_sbp = tbl_string(
            "{mae_sbp_est}\n({mae_sbp_lwr}, {mae_sbp_upr})",
            decimals = c(2, 2, 1)
          ),
          tbv_dbp = tbl_string(
            "{mae_dbp_est}\n({mae_dbp_lwr}, {mae_dbp_upr})",
            decimals = c(2, 2, 1)
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
  
  tbl_data %>% 
    map(
      ~ .x %>% 
        select(n_msr, descr, starts_with('tbv')) %>% 
        as_grouped_data(groups = 'n_msr') %>% 
        as_flextable(hide_grouplabel = TRUE) %>% 
        add_header_row(
          values = c("BP sampling\nvariation", 
            "Kappa statistic (95% CI)", 
            "Mean absolute error (95% CI)\nfor mean systolic BP during sleep", 
            "Mean absolute error (95% CI)\nfor mean diastolic BP during sleep"
          ),
          colwidths = c(1, 3, 3, 3)
        ) %>% 
        set_header_labels(
          descr = '',
          tbv_kap_Overall = "Overall",
          tbv_sbp_Overall = "Overall",
          tbv_dbp_Overall = "Overall",
          tbv_kap_CARDIA = "CARDIA",
          tbv_sbp_CARDIA = "CARDIA",
          tbv_dbp_CARDIA = "CARDIA",
          tbv_kap_JHS = "JHS",
          tbv_sbp_JHS = "JHS",
          tbv_dbp_JHS = "JHS"
        ) %>% 
        theme_box() %>% 
        width(width = 0.92) %>% 
        width(j = 1, width = 1.5) %>% 
        align(align = 'center', part = 'all') %>% 
        align(j = 1, align = 'left') %>% 
        merge_at(i = 1:2, j = 1, part = 'header') 
    )
  
  

}
