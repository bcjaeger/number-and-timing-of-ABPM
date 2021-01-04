##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param kappa_comparisons_wrt_234
##' @param design_evaluated
make_tblkappa_234 <- function(kappa_comparisons_wrt_234, design_evaluated) {

  ref_samplers <- 
    c("3 BP measurements at 2, 3 and 4 hours after midnight",
      "3 BP measurements at 2, 3 and 4 hours after falling asleep")
  
  data_summary <- design_evaluated %>% 
    filter(ID %in% ref_samplers) %>% 
    transmute(ID, 
              study,
              reference = NA_character_,
              lwr = kap_nht_lwr, 
              upr = kap_nht_upr,
              est = kap_nht_est,
              pval = 1) %>% 
    bind_rows(kappa_comparisons_wrt_234) %>% 
    select(-reference)
  
  data_summary %>% 
    transmute(
      ID,
      study,
      n_msr = as.integer(str_sub(ID, start = 1, end = 1)),
      strategy = if_else(
        str_detect(ID, pattern = 'starting at'),
        true = 'cnctr',
        false = 'distr'
      ),
      tbl_value = table_glue('{est}\n({lwr}, {upr})'),
      tbl_value = if_else(
        ID %in% ref_samplers,
        true = str_replace(tbl_value, '\\(.+\\)', "(reference)"),
        false = tbl_value
      )
    ) %>% 
    ID_decompose() %>% 
    mutate(
      strat_by = if_else(
        str_detect(descr, 'midnight$'),
        true = 'midnight', 
        false = 'sleep'
      ),
      strat_by = factor(strat_by, levels = c('sleep', 'midnight')),
      descr = str_remove(descr, ' hours.+$'),
      n_msr = str_replace(n_msr, 'D', 'd'),
      n_msr = str_replace(n_msr, 'C', 'c')
    ) %>% 
    select(-strategy) %>% 
    pivot_wider(names_from = study,
                values_from = tbl_value) %>% 
    unite(n_msr, descr, col = 'n_msr', sep = ' ') %>% 
    select(
      n_msr,
      strat_by,
      Overall,
      CARDIA,
      JHS
    )
  
}
