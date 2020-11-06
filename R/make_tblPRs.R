##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param study_analyzed
##' @param winning_samplers
make_tblPRs <- function(study_analyzed, winning_samplers) {
  
  pr_tbl_data <- study_analyzed %>% 
    mutate(
      mdl = if_else(
        study == 'Overall',
        true = map(mdl, update, . ~ . - study:.),
        false = mdl
      ),
      mdl_tbl = map(mdl, tidy, conf.int = TRUE, exponentiate = TRUE)
    ) %>% 
    unnest(mdl_tbl)
  
  pr_tbl_list <- pr_tbl_data %>% 
    filter(str_detect(term, 'bp_[\\S]{4}$')) %>% 
    mutate(
      ID = fct_relevel(ID, 'Measuring BP throughout sleep'), 
      pr_string = tbl_string(
        '{estimate}\n({conf.low}, {conf.high})', 
        decimals = c(2, 2, 1)
      ),
      pr_pval = tbl_pval(p.value)
    ) %>% 
    select(ID, study, n_msr, strategy, outcome, term, pr_string, pr_pval) %>%  
    group_by(outcome, term) %>% 
    nest() %>% 
    mutate(
      data = map(
        .x = data, 
        .f = ~ .x %>% 
          pivot_wider(
            names_from = c(study), 
            values_from = c(pr_string, pr_pval)
          ) %>% 
          select(ID, n_msr, strategy, contains("Overall"), 
            contains("CARDIA"), contains("JHS"))
      ),
      term = str_replace(term, 'slp_', ''),
      term = str_replace(term, '_samp$|_full$', '')
    ) %>% 
    group_by(outcome, term) %>% 
    summarize(data = list(arrange(bind_rows(data), ID)), .groups = 'drop') %>% 
    unite(outcome, term, col = 'key') %>% 
    deframe()
  
  pr_tbl_winners <- pr_tbl_list %>% 
    map(filter, ID %in% winning_samplers | str_detect(ID, '^Meas')) %>% 
    set_names(glue("{names(.)}_winners"))
  
  pr_tbls <- flatten(list(
    winners  = pr_tbl_winners,
    everyone = pr_tbl_list
  )) %>%
    map(
      ~ .x %>%
        mutate(
          n_msr = if_else(
            condition = strategy == 'cnctr',
            true = str_replace(n_msr, 'msr', ' Consecutive BP measurements'),
            false = str_replace(n_msr, 'msr', ' Distributed BP measurements')
          ),
          ID = str_replace(as.character(ID), "\\d BP measurements ", ""),
          ID = str_replace(ID, 'falling asleep', 'sleep')
        ) %>%
        select(-strategy)
    )
  
  out <- map(
    .x = pr_tbls, 
    .f = ~ as_grouped_data(.x, groups = 'n_msr') %>% 
      .[-1, ] %>% 
      as_flextable(hide_grouplabel = TRUE) %>%
      width(width = 1.1) %>%
      width(j = 1, width = 1.5) %>%
      set_header_labels(
        ID = '',
        pr_string_Overall = 'Prevalence ratio',
        pr_pval_Overall = 'P-value',
        pr_string_CARDIA = 'Prevalence ratio',
        pr_pval_CARDIA = 'P-value',
        pr_string_JHS = 'Prevalence ratio',
        pr_pval_JHS = 'P-value'
      ) %>% 
      add_header_row(
        values = c("Blood pressure\nsampling variation", 
          "Overall", "CARDIA", "JHS"),
        colwidths = c(1, 2, 2, 2)
      ) %>% 
      theme_box() %>% 
      align(align = 'center', part = 'all') %>% 
      align(j = 1, align = 'left', part = 'all') %>% 
      merge_at(i = 1:2, j=1, part = 'header')
  )
  
  
}
