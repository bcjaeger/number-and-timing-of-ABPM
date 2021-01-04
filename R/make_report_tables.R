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
  tbl_kappa_comparisons_wrt_234,
  tbl_exclusions,
  tbl_variations,
  tbl_prs,
  tbl_cstats,
  control_vars
) {
  
  # Setup -------------------------------------------------------------------
  
  tbls_main <- tbls_supp <- tibble(
    object = list(), 
    caption = NA_character_
  )
  
  abbrevs <- c(
    CARDIA = 'Coronary Artery Risk Development in Young Adults',
    CI = 'confidence interval',
    C = 'concordance',
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
    '(i.e., 2, 3, or 4) using the same strategy (i.e., consecutive ', 
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
    " systolic blood pressure \u2265120 mm Hg",
    " or asleep diastolic blood pressure \u226570 mm Hg."
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
    object = list(tbl_variations_flex), 
    caption = glue("Summary of 12 groups of blood pressure sampling variations.")
  ) 
  
  # Characteristics ---------------------------------------------------------
  
  tbl_one <- tibble_one(tbl_characteristics$table, 
                        formula = ~ . | study, 
                        include_pval = TRUE)
  
  tbl_characteristics_flex <- tbl_one %>% 
    to_word() %>%
    width(width = 1.2) %>%
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
    rename(p_value = `P-value`) %>% 
    as_inline(tbl_variables = 'variable', 
              tbl_values = c('Overall', 'CARDIA', "JHS", "p_value")) 
  
  tbls_main %<>% add_row(
    object = list(tbl_characteristics_flex), 
    caption = "Participant characteristics in the overall population and stratified by study."
  ) 
  
  # BP sampler accuracy -----------------------------------------------------
  
  tbl_accuracy_inline <- tbl_accuracy %>%
    map(
      ~ .x %>% 
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
    )
  
  tbl_accuracy_inline_winners <- tbl_accuracy_inline$winners 
  tbl_accuracy_inline_everyone <- tbl_accuracy_inline$everyone 
    
  
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
            write_abbrevs(abbrevs[c('CARDIA', 'BP', 'CI', 'JHS')])
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
    caption = "Kappa statistics and mean absolute error for the blood pressure sampling variation that obtained the highest overall chance-corrected agreement (i.e., Kappa statistic) with ambulatory blood pressure monitoring throughout sleep within each of the 12 categories defined by number of measurements, sampling strategy and time structure."
  ) 
  
  
  
  tbls_supp %<>% add_row(
    object = list(tbl_accuracy_flex$everyone), 
    caption = "Kappa statistics and mean absolute error for all 74 evaluated blood pressure sampling variations."
  ) 
  


  # BP sampler kappa comparisons --------------------------------------------
  
  
  inline_kappa_comparisons_wrt_234 <- 
    tbl_kappa_comparisons_wrt_234 %>% 
    mutate(
      n_msr = str_remove(n_msr, 'BP '),
      n_msr = str_remove(n_msr, 'measurements at '),
      n_msr = str_remove_all(n_msr, ','),
      n_msr = paste("BP", n_msr),
      n_msr = str_replace_all(n_msr, ' ', '_')
    ) %>% 
    mutate(across(c(Overall, CARDIA, JHS), str_replace, '\n', ' ')) %>% 
    as_inline(tbl_variables = c('n_msr', 'strat_by'),
              tbl_values = c('Overall', 'CARDIA', 'JHS'))
  
  tbl_kappa_comparisons_wrt_234_flex <- 
    tbl_kappa_comparisons_wrt_234 %>%
    arrange(strat_by) %>%
    mutate(
      strat_by = fct_recode(
        strat_by,
        'Time is measured in hours after falling asleep' = 'sleep',
        'Time is measured in hours after midnight' = 'midnight'
      )
    ) %>%
    as_grouped_data(groups = 'strat_by') %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    theme_box() %>%
    set_header_labels(n_msr = 'BP sampling variation') %>%
    width(width = 1.25) %>%
    width(j = 1, width = 2.3) %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'body') %>% 
    bg(i = ~ !is.na(strat_by), bg = 'grey80') %>% 
    italic(i = ~ !is.na(strat_by), italic = TRUE) %>% 
    height(height = 1.5, part = 'header') %>%
    footnote(
      i = 1,
      j = 1,
      ref_symbols = '',
      value = as_paragraph(
        write_abbrevs(abbrevs[c('CARDIA', 'BP', 'JHS')])
      )
    ) %>% 
    footnote(
      i = c(2, 10), 
      j = 1,  
      part = 'body', 
      ref_symbols = fts[1],
      value = as_paragraph(
        'Because of its use in previous studies, the Kappa statistic obtained',
        ' by this blood pressure sampling variation is a reference value for',
        ' other blood pressure sampling variations that use the',
        ' same time definition (i.e., hours since falling asleep',
        ' or hours since midnight).'
      )
    ) %>% 
    footnote(
      i = c(2, 3), 
      j = 2,  
      part = 'body', 
      ref_symbols = fts[2],
      value = as_paragraph(
        'Table values are Kappa statistic for the referent blood pressure',
        ' sampling variations and the change in Kappa statistic',
        ' (95% confidence interval) relative to the reference for',
        ' non-referent blood pressure sampling variations.'
      )
    ) %>% 
    footnote(i = 1, j = 1, part = 'header', ref_symbols = '', 
             value = footnote_kappa_defn) %>% 
    footnote(i = 1, j = 1, part = 'header', ref_symbols = '', 
             value = footnote_nht_defn)
  
  tbls_main %<>% add_row(
    object = list(tbl_kappa_comparisons_wrt_234_flex), 
    caption = "Difference (95% confidence interval) in chance-corrected agreement (Kappa statistic) with a full night of ambulatory blood pressure monitoring in classification of nocturnal hypertension for the best blood pressure sampling variation within each of the 12 categories defined by number of measurements, sampling strategy and time structure versus sampling blood pressure at 2, 3, and 4 hours after falling asleep or midnight."
  )
  
  # BP sampler prevalence ratios --------------------------------------------
  control_labs <- recode(
    control_vars,
    "age" = "participant age",
    "sex" = "sex",
    "diabetes" = "diabetes status",
    "currentsmoker" = "smoking status",
    "bpmeds" = "antihypertensive medication use",
    "slp_duration" = "sleep duration",
    "cln_sbp" = "systolic blood pressure in the clinical setting",
    "cln_dbp" = "diastolic blood pressure in the clinical setting"
  )
  
  control_footer <- paste(
    "Prevalence ratios are adjusted for",
    glue_collapse(control_labs, sep = ', ', last = ' and ')
  ) %>% 
    as_paragraph()
  
  inline_prs <- tbl_prs %>% 
    map(
      ~ .x %>% 
        mutate(
          ID = str_replace_all(ID, ' |, ', '_'),
          n_msr = str_replace_all(n_msr, ' ', '_'),
          n_msr = str_remove(n_msr, '_BP_measurements'),
          n_msr = paste0('bp_', n_msr),
          across(.cols = starts_with('pr'), 
                 .fns = str_replace, 
                 pattern = '\n', 
                 replacement = ' ')
        ) %>% 
        pivot_longer(cols = starts_with('pr')) %>% 
        as_inline(tbl_variables = c("ID", "name"),
                  tbl_values = 'value')
    )
  
  .pr_flex_table_maker <- function(outcome, exposure, group, data){
    
    flex_object <- data %>% 
      as_grouped_data(groups = 'n_msr') %>%
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
      merge_at(i = 1:2, j=1, part = 'header') %>% 
      bg(i = ~ !is.na(n_msr), bg = 'grey80') %>% 
      italic(i = ~ !is.na(n_msr), italic = TRUE) %>% 
      height(height = 1.5, part = 'header') %>% 
      footnote(i = 1, j = 1, ref_symbols = '', value = as_paragraph(
        write_abbrevs(abbrevs[c('CARDIA', 'JHS')])))
    
    if(outcome == 'alb'){
      footnote_outcome_defn <- as_paragraph(
        "Albuminuria was defined as an albumin-to-creatinine",
        " ratio \u226530 mg/g"
      )
    } else {
      footnote_outcome_defn <- as_paragraph(
        "Left ventricular hypertrophy was defined as a left ventricular",
        " mass index >95 g/m2 in women and >115 g/m2 in men."
      )
    }
    
    flex_object %<>% footnote(
      i = 1,
      j = 1,
      ref_symbols = '',
      value = footnote_outcome_defn,
      part = 'header'
    )
    
    fts_counter <- 1
    
    if(group == 'winners'){
      
      flex_object %<>% footnote(
        i = 1,
        j = 1,
        ref_symbols = fts[fts_counter],
        value = footnote_winners,
        part = 'header'
      )
      fts_counter <- fts_counter + 1
    }
    
    flex_object %<>%
      footnote(
        i = 2,
        j = c(2, 4, 6),
        ref_symbols = fts[fts_counter],
        value = control_footer,
        part = 'header'
      )
    
    fts_counter <- fts_counter + 1
    
    if(exposure == 'sbp'){
      exposure_footer <- as_paragraph(
        "Prevalence ratios correspond to 10 mm Hg higher ",
        "systolic blood pressure"
      )
    } else {
      exposure_footer <- as_paragraph(
        "Prevalence ratios correspond to 5 mm Hg higher ",
        "diastolic blood pressure"
      )
    }
    
    flex_object %<>% 
      footnote(
        i = 2,
        j = c(2, 4, 6),
        ref_symbols = fts[fts_counter],
        part = 'header',
        value = exposure_footer
      )
    
    fts_counter <- fts_counter + 1
    
    flex_object
    
  }
  
  tbl_prs_flex <- enframe(tbl_prs) %>% 
    mutate(name = str_replace(name, 'bp$', 'bp_everyone')) %>% 
    separate(name, into = c('outcome', 'exposure', 'group')) %>% 
    rowwise() %>% 
    mutate(tbl = list(
      .pr_flex_table_maker(outcome, exposure, group, value)
    )) %>% 
    unite(col = 'id', outcome, exposure, group, sep = '_') %>% 
    select(-value) %>% 
    deframe()
  
  tbls_main %<>% add_row(
    object = list(tbl_prs_flex$lvh_sbp_winners), 
    caption = "Prevalence ratios (95% confidence intervals) for left ventricular hypertrophy associated with mean systolic blood pressure.")
  
  tbls_supp %<>% add_row(
    object = list(tbl_prs_flex$alb_sbp_winners), 
    caption = "Prevalence ratios (95% confidence intervals) for albuminuria associated with mean systolic blood pressure.")
  
  
  # BP sampler C-stats ------------------------------------------------------
  
  inline_cstats <- tbl_cstats %>% 
    map(
      ~ .x %>% 
        mutate(
          ID = str_replace_all(ID, ' |, ', '_'),
          n_msr = str_replace_all(n_msr, ' ', '_'),
          n_msr = str_remove(n_msr, '_BP_measurements'),
          n_msr = paste0('bp_', n_msr),
          across(.cols = starts_with('cstat'), 
                 .fns = str_replace, 
                 pattern = '\n', 
                 replacement = ' ')
        ) %>% 
        pivot_longer(cols = starts_with('cstat')) %>% 
        as_inline(tbl_variables = c("ID", "name"),
                  tbl_values = 'value')
    )
  
  
  .cstat_table_maker <- function(outcome, group, data){
    
    flex_object <- as_grouped_data(data, groups = 'n_msr') %>% 
      .[-1, ] %>% 
      as_flextable(hide_grouplabel = TRUE) %>%
      width(width = 1.1) %>%
      width(j = 1, width = 1.5) %>%
      set_header_labels(
        ID = '',
        cstat_string_Overall = 'C-statistic\n(95% CI)',
        cstat_pval_Overall = 'P-value for\ndifference',
        cstat_string_CARDIA = 'C-statistic\n(95% CI)',
        cstat_pval_CARDIA = 'P-value for\ndifference',
        cstat_string_JHS = 'C-statistic\n(95% CI)',
        cstat_pval_JHS = 'P-value for\ndifference'
      ) %>% 
      add_header_row(
        values = c("Blood pressure\nsampling variation", 
                   "Overall", "CARDIA", "JHS"),
        colwidths = c(1, 2, 2, 2)
      ) %>% 
      theme_box() %>% 
      align(align = 'center', part = 'all') %>% 
      align(j = 1, align = 'left', part = 'all') %>% 
      bg(i = ~ !is.na(n_msr), bg = 'grey80') %>% 
      merge_at(i = 1:2, j=1, part = 'header') %>% 
      italic(i = ~ !is.na(n_msr), italic = TRUE) %>% 
      height(height = 1.5, part = 'header') %>% 
      footnote(i = 1, j = 1, ref_symbols = '', value = as_paragraph(
        write_abbrevs(abbrevs[c('CARDIA', 'BP', 'C', 'CI', 'JHS')])))
    
    if(outcome == 'alb'){
      footnote_outcome_defn <- as_paragraph(
        "Albuminuria was defined as an albumin-to-creatinine",
        " ratio \u226530 mg/g"
      )
    } else {
      footnote_outcome_defn <- as_paragraph(
        "Left ventricular hypertrophy was defined as a left ventricular",
        " mass index >95 g/m2 in women and >115 g/m2 in men."
      )
    }
    
    flex_object %<>% footnote(
      i = 1,
      j = 1,
      ref_symbols = '',
      value = footnote_outcome_defn,
      part = 'header'
    )
    
    fts_counter <- 1
    
    
    if(group == 'winners'){
      
      flex_object %<>% footnote(
        i = 1,
        j = 1,
        ref_symbols = fts[fts_counter],
        value = footnote_winners,
        part = 'header'
      )
      
      fts_counter <- fts_counter + 1
      
    }
    
    flex_object %<>% footnote(
      i = 2,
      j = 1,
      ref_symbols = fts[fts_counter],
      value = as_paragraph('Foregoing blood pressure measurement',
                           ' indicates omission of any term in the model',
                           ' predictors that corresponds to mean blood', 
                           ' pressure during sleep'),
      part = 'body'
    )
    
    fts_counter <- fts_counter + 1
    
    flex_object %<>%
      footnote(
        i = 2, j = 2, part = 'header', ref_symbols = fts[fts_counter],
        value = as_paragraph(
          'All concordance statistics obtained from blood pressure ',
          'sampling variations were compared to ',
          'the concordance statistic obtained when blood pressure was ',
          'measured throughout sleep.'
        )
      ) 
    
    fts_counter <- fts_counter + 1
    
    flex_object %<>% 
      footnote(
        i = 2, j = 3, part = 'header', ref_symbols = fts[fts_counter],
        value = as_paragraph('P-values were obtained using ',
                             'DeLong\'s test for correlated concordance statistics.')
      )
    

    flex_object
    
  }
  
  tbl_cstats_flex <- enframe(tbl_cstats) %>% 
    mutate(name = if_else(str_detect(name, 'winners'),
                          name, paste0(name, '_everyone'))) %>% 
    separate(name, into = c('outcome', 'group')) %>% 
    rowwise() %>% 
    mutate(table = list(.cstat_table_maker(outcome, group, value))) %>% 
    unite(col = 'id', outcome, group) %>% 
    select(-value) %>% 
    deframe()
  
  tbls_main %<>% add_row(
    object = list(tbl_cstats_flex$lvh_winners), 
    caption = "Concordance statistics for left-ventricular hypertrophy in a multivariable-adjusted model.") 
  
  tbls_supp %<>% add_row(
    object = list(tbl_cstats_flex$alb_winners), 
    caption = "Concordance statistics for albuminuria in a multivariable-adjusted model")
  
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
    accuracy_winners = tbl_accuracy_inline_winners,
    accuracy_everyone = tbl_accuracy_inline_everyone,
    prs = inline_prs,
    cstats = inline_cstats,
    kappa_234 = inline_kappa_comparisons_wrt_234
  )
  
  # combine table objects and inline output ---------------------------------
  
  list(tables = tbl_output,
       inline = inline_output)
  
}
