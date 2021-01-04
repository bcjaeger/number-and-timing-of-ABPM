##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param study_analyzed
##' @param winning_samplers
make_tblPRs <- function(study_analyzed, winning_samplers) {
  
  rspec <- round_spec() %>% 
    round_half_even() %>% 
    round_using_decimal(digits = 2)
  
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
      ID = fct_relevel(ID, 'Full night of ABPM'), 
      pr_string = table_glue(
        '{estimate}\n({conf.low}, {conf.high})', 
        rspec = rspec
      ),
      pr_pval = table_pvalue(p.value)
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
    map(filter, ID %in% winning_samplers | str_detect(ID, '^Full night')) %>% 
    set_names(glue("{names(.)}_winners"))
  
  flatten(list(
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
          ID = str_replace(as.character(ID), "\\d BP measurements ", "")
        ) %>%
        select(-strategy)
    )
  
}
