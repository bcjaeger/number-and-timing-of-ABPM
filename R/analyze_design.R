##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design_init
##' @param abpm_wide
##' @param control_vars

analyze_design <- function(design_init, abpm_wide, control_vars) {

  as_01 <- function(x) as.integer(tolower(x) == 'yes')
  
  control_jhs <- control_vars
  control_cardia <- c(control_vars, 'race')
  control_overall <- control_vars
  
  outcomes <- c('lvh', 'albuminuria')
  .variables <- c(outcomes, control_cardia)
  
  # prep data for modeling --------------------------------------------------
  
  analysis <- abpm_wide %>% 
    mutate(across(all_of(outcomes), as_01)) %>% 
    mutate(
      across(matches('\\_sbp\\_'), ~.x / 10),
      across(matches('\\_dbp\\_'), ~.x / 5)
    ) %>% 
    mutate(across(where(is.character), as.factor))
  
  # make formulas for analysis ----------------------------------------------
  
  f_make <- function(outcome, exposure_var, control_vars, int_var = NULL){
    rhs_vars <- c(exposure_var, control_vars)
    rhs <- glue_collapse(rhs_vars, sep = ' + ')
    if(!is.null(int_var)) rhs <- glue("({rhs}) * {int_var}")
    as.formula(glue("{outcome} ~ {rhs}"))
  }
  
  exposure_full <- c('slp_sbp_full', 'slp_dbp_full')
  exposure_samp <- c('slp_sbp_samp', 'slp_dbp_samp')
  exposure_rdcd <- NULL
  
  formulas_full <- list(
    Overall_lvh = f_make('lvh', exposure_full, control_overall, 'study'),
    Overall_alb = f_make('albuminuria', exposure_full, control_overall, 'study'),
    CARDIA_lvh  = f_make('lvh', exposure_full, control_cardia),
    CARDIA_alb  = f_make('albuminuria', exposure_full, control_cardia),
    JHS_lvh     = f_make('lvh', exposure_full, control_jhs),
    JHS_alb     = f_make('albuminuria', exposure_full, control_jhs)
  ) %>% 
    enframe(value = 'formula') %>% 
    separate(name, into = c('study', 'outcome'))
  
  formulas_samp <- list(
    Overall_lvh = f_make('lvh', exposure_samp, control_overall, 'study'),
    Overall_alb = f_make('albuminuria', exposure_samp, control_overall, 'study'),
    CARDIA_lvh  = f_make('lvh', exposure_samp, control_cardia),
    CARDIA_alb  = f_make('albuminuria', exposure_samp, control_cardia),
    JHS_lvh     = f_make('lvh', exposure_samp, control_jhs),
    JHS_alb     = f_make('albuminuria', exposure_samp, control_jhs)
  ) %>% 
    enframe(value = 'formula') %>% 
    separate(name, into = c('study', 'outcome'))
  
  formulas_rdcd <- list(
    Overall_lvh = f_make('lvh', exposure_rdcd, control_overall, 'study'),
    Overall_alb = f_make('albuminuria', exposure_rdcd, control_overall, 'study'),
    CARDIA_lvh  = f_make('lvh', exposure_rdcd, control_cardia),
    CARDIA_alb  = f_make('albuminuria', exposure_rdcd, control_cardia),
    JHS_lvh     = f_make('lvh', exposure_rdcd, control_jhs),
    JHS_alb     = f_make('albuminuria', exposure_rdcd, control_jhs)
  ) %>% 
    enframe(value = 'formula') %>% 
    separate(name, into = c('study', 'outcome'))
  
  formulas_all <- bind_rows(
    full = formulas_full,
    samp = formulas_samp,
    rdcd = formulas_rdcd,
    .id = 'model_type'
  ) 
  
  # merge study design data with formulas -----------------------------------
  
  .design_init <- design_init %>% 
    mutate(
      bp_data = map(
        .x = bp_means, .f = mutate, 
        across(matches('\\_sbp\\_'), ~.x / 10),
        across(matches('\\_dbp\\_'), ~.x / 5)
      ),
      bp_data = map(bp_data, left_join, analysis, by = 'subjid')
    ) %>% 
    select(ID, study, time_var, strategy, n_msr, bp_data) %>%
    add_row(ID = 'Full night of ABPM', study = 'Overall', 
      bp_data = list(analysis)) %>% 
    add_row(ID = 'Full night of ABPM', study = 'CARDIA', 
      bp_data = list(filter(analysis, study == 'CARDIA'))) %>% 
    add_row(ID = 'Full night of ABPM', study = 'JHS', 
      bp_data = list(filter(analysis, study == 'JHS'))) %>% 
    add_row(ID = 'Foregoing BP measurement', study = 'Overall', 
      bp_data = list(analysis)) %>%
    add_row(ID = 'Foregoing BP measurement', study = 'CARDIA', 
      bp_data = list(filter(analysis, study == 'CARDIA'))) %>% 
    add_row(ID = 'Foregoing BP measurement', study = 'JHS', 
      bp_data = list(filter(analysis, study == 'JHS'))) %>% 
    mutate(
      model_type = case_when(
        ID == 'Full night of ABPM' ~ 'full',
        ID == 'Foregoing BP measurement' ~ 'rdcd',
        TRUE ~ 'samp'
      )
    ) %>% 
    left_join(formulas_all) %>% 
    select(-model_type)
  
  # analyze study design, fitting models to each row ------------------------
  
  gee_fitter <- function(formula, data, family = 'poisson', ...){
    
    .dots <- list(...)
    
    if ("id" %in% names(.dots)) .dots$id = NULL
    
    rhs <- attr(terms(formula), 'term.labels') %>% 
      str_split("\\:") %>% 
      reduce(base::c) %>% 
      unique()
    
    mdl_data <- drop_na(data, !!!rhs)
    
    ids = 1:nrow(mdl_data)
    
    .dots$id = ids
    .dots$data = mdl_data
    .dots$family = family
    .dots$formula = formula
    
    do.call(
      what = geepack::geeglm,
      args = .dots
    )
    
  }
  
  gee_cstat <- function(gee_fit){
    
    outcome <- as.character(gee_fit$formula[[2]])
    
    .fitted = as.numeric(predict(gee_fit, type = 'response'))
    .outcome = gee_fit$model[[outcome]]
    
    auc(pROC::roc(
      response  = .outcome, 
      predictor = .fitted,
      levels = c(0,1),
      direction = '<'
    ))
    
  }

  .design_init %>% 
    mutate(
      mdl = map2(formula, bp_data, gee_fitter),
      cstat = map(mdl, gee_cstat),
      cstat_tbl = map(
        .x = cstat, 
        .f = ~ ci.auc(.x) %>% 
          .[c(2,1,3)] %>% 
          set_names(c('est', 'lwr', 'upr'))
      ),
      mdl_tbl = map(mdl, tidy, exponentiate = TRUE, conf.int = TRUE)
    )

}
