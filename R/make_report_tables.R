##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param tbl_exclusions
##' @param tbl_variations
##' @param 
make_report_tables <- function(tbl_exclusions,
                               tbl_variations,
                               tbl_characteristics) {

  # Setup -------------------------------------------------------------------
  
  tbls_main <- tbls_supp <- tibble(
    object = list(), 
    caption = NA_character_
  )
  
  abbrevs <- c(
    CARDIA = 'Coronary Artery Risk Development in Young Adults',
    CI = 'confidence interval',
    BSA = "body surface area",
    BP = 'blood pressure',
    GED = 'General Educational Development',
    JHS = 'Jackson Heart Study',
    ABPM = 'ambulatory blood pressure monitoring',
    BSA = 'body surface area'
  )
  
  fts <- c(
    '\u2A',
    '\u2020',
    '\u2021',
    '\uA7',
    '\u2016',
    '\uB6',
    '#',
    '\u2a\u2a',
    '\u2020\u2020',
    '\u2021\u2021',
    '\u2021\u2021\u2021\u2021'
  )
  
  lab_acc <- "accuracy of different sampling strategies"
  lab_est <- "estimate mean blood pressure during sleep"
  lab_bps <- "blood pressure sampling times"
  lab_cda <- "Coronary Artery Risk Development in Young Adults study"
  lab_cnc <- "consecutive strategy"
  lab_dst <- "distributed strategy"
  lab_jhs <- "Jackson Heart Study"
  lab_msr <- "2-4 measurements"
  lab_prt <- "participants in the"
  lab_wnr <- "the highest kappa statistic"
  lab_pr  <- 'prevalence ratio'
  lab_ci  <- '95% confidence interval'
  lab_nbp <- 'blood pressure during sleep'
  lab_lvh <- 'left ventricular hypertrophy'
  lab_alb <- 'albuminuria'
  lab_abp <- 'ambulatory blood pressure monitoring'
  
  footnote_winners <- as_paragraph(
    'Blood pressure sampling variations were compared to other ',
    'variations that measure blood pressure the same number of times ',
    '(i.e., 2, 3, or 4) using the same strategy (i.e., Consecutive ', 
    'or distributed) and the same time reference (i.e., midnight or ',
    'onset of sleep). Each of these 12 comparison groups had one ',
    'variation with the highest overall Kappa statistic, ',
    'and those variations are presented here.'
  )
  
  footnote_kappa_defn <- as_paragraph(
    'Kappa statistics measure the chance-corrected agreement in ',
    'classification of nocturnal hypertension between ',
    'ambulatory blood pressure monitoring throughout sleep and ', 
    'a blood pressure sampling variation.'
  )
  
  .tsm <- 'Time since midnight, hours'
  .tss <- 'Time since falling asleep, hours'
  
  .cda <- 'CARDIA participants'
  .jhs <- 'JHS participants'
  
  .cnc <- 'consecutive strategy'
  .dst <- 'distributed strategy'
  
  .cap <- function (string) {
    capped <- grep("^[A-Z]", string, invert = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
    return(string)
  }
  
  # Exclusions --------------------------------------------------------------
  
  tbl_exclusions_flex <- tbl_exclusions %>% 
    transmute(
      exclusion,
      across(
        .cols = c(Overall, CARDIA, JHS), 
        .fns = ~table_value(as.integer(.x))
      )
    ) %>% 
    flextable(theme_fun = theme_box) %>% 
    set_header_labels(
      exclusion = 'Inclusion criteria',
      CARDIA = 'CARDIA participants',
      JHS = 'JHS participants'
    ) %>% 
    width(j=1, width=2.8) %>% 
    width(j=2:4, width = 1.2) %>% 
    align(align = 'center', part = 'all') %>% 
    align(j=1, align = 'left', part = 'body') %>% 
    footnote(i = 1, j = 1, ref_symbols = '', value = as_paragraph(
      write_abbrevs(abbrevs[c('CARDIA', 'JHS', 'ABPM')])
    ))
  
  tbl_exclusions_inline <- 
    tbl_exclusions %>% 
    mutate(
      across(
        .cols = c(Overall, CARDIA, JHS), 
        .fns = ~table_value(as.integer(.x))
      )
    ) %>% 
    as_inline(tbl_variables = c('exclusion_abbreviated'),
              tbl_values = c('Overall', 'CARDIA', 'JHS'))
  
  tbl_variations_flex <- tbl_variations %>% 
    flextable(theme_fun = theme_box) %>% 
    set_header_labels(
      description = 'Group description',
      variants = "BP sampling variations"
    ) %>% 
    width(j = 1, width = 2.5) %>% 
    width(j = 2, width = 4.5) %>% 
    align(j = 1, align = 'left', part = 'all') %>% 
    align(j = 2, align = 'right', part = 'all')
  
  tbls_supp %<>% add_row(
    object = list(tbl_exclusions_flex), 
    caption = glue("Participant inclusion cascade.")
  ) 
  
  # Characteristics ---------------------------------------------------------
  
  tbl_characteristics_flex <- tbl_characteristics  %>% 
    to_word() %>% 
    width(width = 4/3) %>% 
    width(j=1, width = 3) %>% 
    footnote(i = 1, j = 1, ref_symbols = '', missing_footer) %>% 
    footnote(i = 1, j = 1, ref_symbols = '', value = as_paragraph(
      write_abbrevs(abbrevs[c('CARDIA', 'BSA', 'GED', 'JHS')])))
  
  tbls_main %<>% add_row(
    object = list(tbl_characteristics_flex), 
    caption = "Participant characteristics overall and stratified by study."
  ) 
  

}
