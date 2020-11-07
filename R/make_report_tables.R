##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param tbl_exclusions
##' @param tbl_variations
##' @param 
make_report_tables <- function(
  tbl_characteristics,
  tbl_accuracy,
  tbl_exclusions,
  tbl_variations
) {
  
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
  
  footnote_diabetes_defn <- as_paragraph(
    "Diabetes was defined as fasting (8+ hours) glucose of at ",
    "least 126 mg/dL or current use of anti-diabetes medication."
  )
  
  footnote_currentsmoker_defn <- as_paragraph(
    "Smoking status was defined as self-reporting ",
    "cigarette use within the past year." 
  )
  
  footnote_nht_defn <- as_paragraph(
    "Nocturnal hypertension was defined as asleep",
    "systolic/diastolic blood pressure \u2265120/70 mm Hg."
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
  
  tbls_supp %<>% add_row(
    object = list(tbl_exclusions_flex), 
    caption = glue("Participant inclusion cascade.")
  ) 
  
  # Summary of BP samplers --------------------------------------------------
  
  tbl_variations_flex <- tbl_variations %>% 
    flextable(theme_fun = theme_box) %>% 
    set_header_labels(
      description = 'Group description',
      variants = "BP sampling variations"
    ) %>% 
    width(j = 1, width = 2.5) %>% 
    width(j = 2, width = 4.5) %>% 
    align(align = 'center', part = 'header') %>% 
    align(align = 'left', part = 'body')
  
  tbls_supp %<>% add_row(
    object = list(tbl_exclusions_flex), 
    caption = glue("Summary of 12 groups of blood pressure sampling variations.")
  ) 
  
  # Characteristics ---------------------------------------------------------
  
  tbl_one <- tibble_one(tbl_characteristics$table, 
                        formula = ~ . | study)
  
  tbl_characteristics_flex <- to_word(tbl_one) %>%
    width(width = 4 / 3) %>%
    width(j = 1, width = 3) %>%
    footnote(
      i = 1,
      j = 1,
      ref_symbols = '',
      value = tbl_characteristics$missing_footer
    ) %>%
    footnote(
      i = 1,
      j = 1,
      ref_symbols = '',
      value = as_paragraph(
        write_abbrevs(abbrevs[c('CARDIA', 'BSA', 'GED', 'JHS')])
      )
    )
  
  tbl_characteristics_inline <- tbl_one %>% 
    as_inline(tbl_variables = 'variable', 
              tbl_values = c('Overall', 'CARDIA', "JHS")) 
  
  tbls_main %<>% add_row(
    object = list(tbl_characteristics_flex), 
    caption = "Participant characteristics in the overall population and stratified by study."
  ) 
  
  # BP sampler accuracy -----------------------------------------------------
  
  tbl_accuracy_inline <- tbl_accuracy$winners %>% 
    mutate(
      descr = str_replace_all(descr, ' |, ', '_'),
      n_msr = str_replace_all(n_msr, ' ', '_'),
      n_msr = str_remove(n_msr, '_BP_measurements'),
      n_msr = paste0('bp_', n_msr),
      across(.cols = starts_with('tbv'), 
             .fns = str_replace, 
             pattern = '\n', 
             replacement = ' ')
    ) %>% 
    as_inline(tbl_variables = c('n_msr', 'descr'),
              tbl_values = c("tbv_kap_Overall", "tbv_kap_CARDIA", 
                             "tbv_kap_JHS", "tbv_sbp_Overall", 
                             "tbv_sbp_CARDIA", "tbv_sbp_JHS", 
                             "tbv_dbp_Overall", "tbv_dbp_CARDIA", 
                             "tbv_dbp_JHS"))
  
  tbl_accuracy_flex <- tbl_accuracy %>% 
    map(
      ~ .x %>% 
        select(n_msr, descr, starts_with('tbv')) %>% 
        as_grouped_data(groups = 'n_msr') %>% 
        as_flextable(hide_grouplabel = TRUE) %>% 
        add_header_row(
          values = c("BP sampling\nvariation", 
                     "Kappa statistic (95% CI)\nfor nocturnal hypertension", 
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
        width(width = 0.95) %>%
        width(j = 1, width = 1.5) %>%
        align(align = 'center', part = 'all') %>%
        align(j = 1, align = 'left') %>%
        merge_at(i = 1:2, j = 1, part = 'header') %>%
        bg(i = ~ !is.na(n_msr), bg = 'grey80') %>%
        italic(i = ~ !is.na(n_msr), italic = TRUE) %>%
        height(height = 1.5, part = 'header') %>%
        footnote(
          i = 1,
          j = 1,
          ref_symbols = '',
          value = as_paragraph(
            write_abbrevs(abbrevs[c('CARDIA', 'BP', 'JHS')])
          )
        )
    )
  
  tbl_accuracy_flex$winners %<>% 
    footnote(i = 1:2, j = 1, part = 'header', ref_symbols = fts[1],
             value = footnote_winners) %>% 
    footnote(i = 1, j = 2, part = 'header', ref_symbols = fts[2], 
             value = footnote_kappa_defn) %>% 
    footnote(i = 1, j = 2, part = 'header', ref_symbols = fts[3], 
             value = footnote_nht_defn)
  
  tbl_accuracy_flex$everyone %<>% 
    footnote(i = 1, j = 2, part = 'header', ref_symbols = fts[1], 
             value = footnote_kappa_defn) %>% 
    footnote(i = 1, j = 2, part = 'header', ref_symbols = fts[2], 
             value = footnote_nht_defn)
  
  tbls_main %<>% add_row(
    object = list(tbl_accuracy_flex$winners), 
    caption = "summary of 12 blood pressure sampling variations that obtained the highest overall chance-corrected agreement (i.e., Kappa statistic) with ambulatory blood pressure monitoring throughout sleep."
  ) 
  
  tbls_supp %<>% add_row(
    object = list(tbl_accuracy_flex$everyone), 
    caption = "Summary of all 74 blood pressure sampling variations that were evaluated in the current study."
  ) 
  
  # Format and bind ---------------------------------------------------------
  
  tbl_output <- bind_rows(table_main = tbls_main,
                          table_supplement = tbls_supp,
                          .id = 'split_me') %>%
    group_by(split_me) %>%
    mutate(
      pre_cap = glue("Table {1:n()}"),
      pre_cap = if_else(split_me == 'table_supplement',
                        true = str_replace(pre_cap, 'Table ', 'Table S'),
                        false = as.character(pre_cap)),
      object = map(object,
                   table_polisher,
                   font_size = 12,
                   font_name = "Times New Roman")
    )
  
  inline_output <- list(
    characteristics = tbl_characteristics_inline,
    exclusions = tbl_exclusions_inline,
    accuracy = tbl_accuracy_inline
  )
  
  # combine table objects and inline output ---------------------------------
  
  list(tables = tbl_output,
       inline = inline_output)
  
}
