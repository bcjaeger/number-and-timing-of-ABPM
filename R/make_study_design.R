##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param abpm_long
##' @param nht_sbp
##' @param nht_dbp

make_study_design <- function(abpm_long, nht_sbp, nht_dbp) {

  lwr_time=1
  upr_time=5
  
  ntimes=c(2,3,4)
  
  list_cols <- function(.matrix){
    output <- vector(mode = 'list', length = ncol(.matrix))
    for(i in seq(ncol(.matrix))) output[[i]] <- as.numeric(.matrix[,i])
    output
  }
  
  
  distr = map(ntimes, ~ combn(lwr_time:upr_time, .x)) %>%
    map(list_cols) %>%
    enframe(name = NULL, value = 'sample_times') %>% 
    unnest(sample_times) %>% 
    mutate(
      n_msr = map_chr(sample_times, length),
      n_msr = paste(n_msr, 'msr', sep = '')
    )
  
  
  # create all Consecutive protocol times ----------------------------------
  
  
  cnctr = list(
    JHS = list(
      msr4 = list(
        rep(1, 4) + c(0, .33, .66, 1),
        rep(2, 4) + c(0, .33, .66, 1),
        rep(3, 4) + c(0, .33, .66, 1),
        rep(4, 4) + c(0, .33, .66, 1)
      ),
      msr3 = list(
        rep(1, 3) + c(0, .33, .66),
        rep(2, 3) + c(0, .33, .66),
        rep(3, 3) + c(0, .33, .66),
        rep(4, 3) + c(0, .33, .66)
      ),
      msr2 = list(
        rep(1, 2) + c(0, .33),
        rep(2, 2) + c(0, .33),
        rep(3, 2) + c(0, .33),
        rep(4, 2) + c(0, .33)
      )
    ),
    CARDIA = list(
      msr4 = list(
        rep(1, 4) + c(0, 1 / 2, 1, 3 / 2),
        rep(2, 4) + c(0, 1 / 2, 1, 3 / 2),
        rep(3, 4) + c(0, 1 / 2, 1, 3 / 2),
        rep(4, 4) + c(0, 1 / 2, 1, 3 / 2)
      ),
      msr3 = list(
        rep(1, 3) + c(0, 1 / 2, 1),
        rep(2, 3) + c(0, 1 / 2, 1),
        rep(3, 3) + c(0, 1 / 2, 1),
        rep(4, 3) + c(0, 1 / 2, 1)
      ),
      msr2 = list(
        rep(1, 2) + c(0, 1 / 2),
        rep(2, 2) + c(0, 1 / 2),
        rep(3, 2) + c(0, 1 / 2),
        rep(4, 2) + c(0, 1 / 2)
      )
    )
  ) %>% 
    enframe(name = 'study', value = 'sample_times') %>% 
    unnest(sample_times) %>% 
    unnest(sample_times) %>% 
    split(f = .$study) %>% 
    map(select, -study) %>% 
    map(mutate, 
      n_msr = map_chr(sample_times, length),
      n_msr = paste(n_msr, 'msr', sep = '')
    )
  
  # combine all study parameters with bp sampling times ---------------------
  
  study_design <- expand.grid(
    study = c("CARDIA", "JHS"),
    time_var = c("tsm", "tss"),
    strategy = c('cnctr', 'distr'),
    stringsAsFactors = FALSE
  ) %>% 
    as_tibble() %>% 
    mutate(
      data = case_when(
        strategy == 'distr'  ~ list(distr),
        study == 'CARDIA' & strategy == 'cnctr' ~ list(cnctr$CARDIA),
        study == 'JHS' & strategy == 'cnctr' ~ list(cnctr$JHS)
      )
    ) %>% 
    unnest(cols = data) %>% 
    mutate(
      ID = pmap_chr(
        .l = list(strategy, sample_times, time_var, n_msr), 
        .f = function(strat, samp, tvar, nmsr) {
          
          time_append <- switch(tvar, 
            'tsm' = 'hours after midnight', 
            'tss' = 'hours after falling asleep'
          )
          
          msr_prepend <- str_replace(nmsr, 'msr', ' BP measurements')
          
          if(strat == 'cnctr') 
            return(
              tbl_string('{msr_prepend} starting at ',
                '{as.integer(samp[1])} {time_append}')
            )
          
          if(strat == 'distr'){
            times_collapsed <- 
              glue_collapse(samp, sep = ', ', last = ' and ')
            return(tbl_string('{msr_prepend} at ',
              '{times_collapsed} {time_append}'))
          }
          
        }
      )
    ) %>% 
    relocate(sample_times, .after = ID) %>% 
    relocate(ID, .before = study)
  
  study_design$bp_means <- list(NULL)
  
  # obtaining sampled bp values ---------------------------------------------
  
  .abpm_long <- split(abpm_long, abpm_long$study)
  
  for(i in seq(nrow(study_design))){
    
    .study    <- as.character(study_design$study[i])
    .time_var <- as.character(study_design$time_var[i])
    .times    <- study_design$sample_times[[i]]
    
    ..times <- glue_collapse(.times, sep = ', ', last = ' and ')
    ..time_var <- switch(.time_var, 
      'tsm' = 'hours after midnight',
      'tss' = 'hours after falling asleep'
    )
    
    cat(
      glue("sampling {.study} BP values at {..times} {..time_var}"),
      '\n'
    )
    
    study_design$bp_means[[i]] <- 
      .abpm_long[[.study]] %>% 
      group_by(subjid) %>% 
      nest() %>% 
      transmute(
        subjid,
        samp = map(data, abp_sample_get, .times, .time_var)
      ) %>% 
      unnest(samp) %>% 
      summarize(
        slp_sbp_samp = mean(sbp), 
        slp_dbp_samp = mean(dbp),
        .groups = 'drop'
      ) %>% 
      mutate(
        nht_samp = if_else(
          slp_sbp_samp >= nht_sbp | slp_dbp_samp >= nht_dbp,
          true = 'yes', 
          false = 'no'
        )
      )
    
  }
  
  pooled_design <- study_design %>% 
    group_by(ID) %>% 
    summarize(time_var = time_var[1], study = 'Overall', 
      strategy = strategy[1], n_msr = n_msr[1], 
      bp_means = list(bind_rows(bp_means)))
  
  study_design %>% 
    select(-sample_times) %>% 
    bind_rows(pooled_design)

}
