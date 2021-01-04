##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param study_analyzed
##' @param winning_samplers
make_tblcstat <- function(study_analyzed, winning_samplers) {
  
  rspec <- round_spec() %>% 
    round_half_even() %>% 
    round_using_magnitude(
      digits = c(3,2,1),
      breaks = c(1, 10, Inf)
    )
  
  cstat_tbl_data <- study_analyzed %>% 
    unnest_wider(cstat_tbl, names_sep = '_') %>% 
    mutate(
      ID = fct_relevel(ID, 
        'Full night of ABPM',
        'Foregoing BP measurement'), 
      cstat_string = table_glue(
        '{cstat_tbl_est}\n({cstat_tbl_lwr}, {cstat_tbl_upr})', 
        rspec = rspec
      )
    ) %>% 
    group_by(study, outcome) %>% 
    mutate(
      cstat_ref = cstat[ID == 'Full night of ABPM'],
      cstat_tst = map2(cstat, cstat_ref, roc.test, paired = TRUE),
      cstat_pval = map_dbl(cstat_tst, 'p.value')
    ) %>% 
    select(ID, study, n_msr, strategy, outcome, cstat_string, cstat_pval) %>%  
    mutate(cstat_pval = table_pvalue(cstat_pval)) %>% 
    group_by(outcome) %>% 
    nest() %>% 
    mutate(
      data = map(
        .x = data, 
        .f = ~ .x %>% 
          pivot_wider(
            names_from = c(study), 
            values_from = c(cstat_string, cstat_pval)
          ) %>% 
          select(ID, n_msr, strategy, contains("Overall"), 
            contains("CARDIA"), contains("JHS"))
      )
    ) %>% 
    deframe()
  
  cstat_tbl_winners <- cstat_tbl_data %>% 
    map(filter, ID %in% winning_samplers | str_detect(ID, '^Fore|^Full')) %>% 
    set_names(glue("{names(.)}_winners"))
  
  flatten(list(
    winners  = cstat_tbl_winners,
    everyone = cstat_tbl_data
  )) %>%
    map(
      ~ .x %>%
        arrange(ID) %>% 
        mutate(
          n_msr = if_else(
            condition = strategy == 'cnctr',
            true = str_replace(n_msr, 'msr', ' Consecutive BP measurements'),
            false = str_replace(n_msr, 'msr', ' Distributed BP measurements')
          ),
          across(
            .cols = contains('cstat_pval'),
            .fns = ~ replace(.x, 
              ID == 'Full night of ABPM', 
              "reference")
          ),
          ID = str_replace(as.character(ID), "\\d BP measurements ", "")
        ) %>%
        select(-strategy)
    )

}


# cstats_overall <- study_analyzed %>% 
#   mutate(
#     across(c(time_var, strategy, n_msr, sample_label),
#       ~coalesce(.x, model_type))
#   ) %>% 
#   split(list(.$sample_label, .$time_var, .$strategy, .$n_msr), 
#     sep = 'x.x', drop = TRUE) %>%
#   map_dfr(
#     ~ {
#       
#       prd <- reduce(map(.x$mdl, predict, type = 'response'), base::c)
#       tru <- reduce(map(.x$mdl, 'y'), base::c)
#       
#       enframe(list(roc(
#         response  = tru, 
#         predictor = prd,
#         levels = c(0,1),
#         direction = '<'
#       )), name = NULL, value = 'cstat')
#       
#     },
#     .id = 'abp_id'
#   ) %>% 
#   separate(abp_id, 
#     into = c('sample_label', 'time_var', 'strategy', 'n_msr'),
#     sep = 'x\\.x') %>% 
#   mutate(
#     cstat_ref = cstat[sample_label == 'full'],
#     cstat_tst = map2(cstat, cstat_ref, roc.test, paired = TRUE),
#     cstat_pval = map_dbl(cstat_tst, 'p.value'),
#     cstat_tbl = map(
#       .x = cstat, 
#       .f = ~ auc(.x) %>% 
#         ci.auc() %>% 
#         .[c(2,1,3)] %>% 
#         set_names(c('est', 'lwr', 'upr'))
#     ),
#     cstat_ostring = map_chr(
#       .x = cstat_tbl, 
#       .f = ~ table_glue("{.x['est']}\n({.x['lwr']}, {.x['upr']})",
#         decimals = c(3, 2, 1))
#     )
#   ) %>% 
#   select(-cstat, -cstat_ref, -cstat_tst, -cstat_tbl)
