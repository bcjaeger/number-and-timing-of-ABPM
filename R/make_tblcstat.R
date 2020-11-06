##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param study_analyzed
##' @param winning_samplers
make_tblcstat <- function(study_analyzed, winning_samplers) {
  
  cstat_tbl_data <- study_analyzed %>% 
    unnest_wider(cstat_tbl, names_sep = '_') %>% 
    mutate(
      ID = fct_relevel(ID, 
        'Measuring BP throughout sleep',
        'Foregoing BP measurement'), 
      cstat_string = tbl_string(
        '{cstat_tbl_est}\n({cstat_tbl_lwr}, {cstat_tbl_upr})', 
        decimals = c(3, 2, 1)
      )
    ) %>% 
    group_by(study, outcome) %>% 
    mutate(
      cstat_ref = cstat[ID == 'Measuring BP throughout sleep'],
      cstat_tst = map2(cstat, cstat_ref, roc.test, paired = TRUE),
      cstat_pval = map_dbl(cstat_tst, 'p.value')
    ) %>% 
    select(ID, study, n_msr, strategy, outcome, cstat_string, cstat_pval) %>%  
    mutate(cstat_pval = tbl_pval(cstat_pval)) %>% 
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
    map(filter, ID %in% winning_samplers | str_detect(ID, '^Fore|^Meas')) %>% 
    set_names(glue("{names(.)}_winners"))
  
  cstat_tbls <- flatten(list(
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
              ID == 'Measuring BP throughout sleep', 
              "reference")
          ),
          ID = str_replace(as.character(ID), "\\d BP measurements ", ""),
          ID = str_replace(ID, 'falling asleep', 'sleep')
        ) %>%
        select(-strategy)
    )
  
  map(
    .x = cstat_tbls, 
    .f = ~ as_grouped_data(.x, groups = 'n_msr') %>% 
      .[-1, ] %>% 
      as_flextable(hide_grouplabel = TRUE) %>%
      width(width = 1.1) %>%
      width(j = 1, width = 1.5) %>%
      set_header_labels(
        ID = 'Blood pressure\nsampling variation',
        cstat_string_Overall = 'C-statistic\n(95% CI)',
        cstat_pval_Overall = 'P-value for\ndifference',
        cstat_string_CARDIA = 'C-statistic\n(95% CI)',
        cstat_pval_CARDIA = 'P-value for\ndifference',
        cstat_string_JHS = 'C-statistic\n(95% CI)',
        cstat_pval_JHS = 'P-value for\ndifference'
      ) %>% 
      add_header_row(
        values = c("", "Overall", "CARDIA", "JHS"),
        colwidths = c(1, 2, 2, 2)
      ) %>% 
      theme_box() %>% 
      align(align = 'center', part = 'all') %>% 
      align(j = 1, align = 'left', part = 'all')
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
#       .f = ~ tbl_string("{.x['est']}\n({.x['lwr']}, {.x['upr']})",
#         decimals = c(3, 2, 1))
#     )
#   ) %>% 
#   select(-cstat, -cstat_ref, -cstat_tst, -cstat_tbl)
