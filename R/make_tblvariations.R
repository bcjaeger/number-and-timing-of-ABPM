##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design_evaluated
make_tblvariations <- function(design_evaluated) {

  design_evaluated %>% 
    filter(study == 'Overall') %>% 
    mutate(
      ID = str_replace(ID,
        pattern = ' hours after midnight| hours after falling asleep',
        replacement = ''),
      ID = str_replace(ID, 
        pattern = '\\d? BP measurements ', 
        replacement = ''),
      n_msr = recode(n_msr, 
        "2msr" = "2",
        "3msr" = "3",
        "4msr" = "4"),
      ID = if_else(
        condition = time_var == 'tsm',
        true = str_replace_all(ID, '(\\d)', '\\1am'),
        false = ID
      ),
      strategy = recode(strategy,
        "cnctr" = " Consecutive BP measurements",
        "distr" = " Distributed BP measurements"),
      time_var = recode(time_var,
        "tsm" = ", hours since midnight",
        "tss" = ", hours since falling asleep")
    ) %>% 
    unite(col = 'description', n_msr, strategy, time_var, sep = '') %>% 
    group_by(description) %>% 
    summarize(
      variants = glue_collapse(ID, sep = '; ', last = '; and '),
      .groups = 'drop'
    ) 
}
