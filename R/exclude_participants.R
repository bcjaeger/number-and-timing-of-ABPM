##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param abpm_long
exclude_participants <- function(abpm_long, threshold) {

  e1 <- abpm_long
  
  e2 <- abpm_long %>% 
    filter(status == 'Asleep', sleep_diary_valid == 1, nreadings_slp >= 5)
  
  e3 <- filter(e2, asleep_1_to_5)
  
  e4 <- e3 %>% 
    nest() %>% 
    mutate(
      valid_sample = map2_lgl(study, data, .f = ~{
        # JHS measured BP every 1/3 hour, CARDIA every 1/2
        delta <- switch(.x, 'CARDIA' = 1/2,'JHS' = 1/3)
        # all times between 1 am and 5am / 1hr to 5hr after sleep
        times <- seq(1, 5, by = delta)
        # check both
        tsm_check <- abp_sample_check(
          data = .y,
          times = times,
          time_variable = 'tsm',
          threshold = threshold)
        tss_check <- abp_sample_check(
          data = .y,
          times = times,
          time_variable = 'tss',
          threshold = threshold)
        # return TRUE if both checks are good
        tsm_check & tss_check
      })
    ) %>% 
    filter(valid_sample) %>% 
    unnest(cols = data)
  
  abpm_long_final <- e4 %>%
    select(-valid_sample,
      -asleep_1_to_5,
      -nreadings_slp,
      -sleep_diary_valid)
  
  exclusion_table <- list(e1, e2, e3, e4) %>% 
    map(~distinct(.x, study, subjid)) %>% 
    map_dfr(~count(ungroup(.x), study), .id = 'exclusion') %>% 
    pivot_wider(values_from = n, names_from = study) %>% 
    add_row(exclusion = '0', CARDIA = 5115, JHS = 5306, .before = 1) %>% 
    mutate(
      exclusion_abbreviated = recode(
        exclusion,
        '0' = 'exc_0_all',
        '1' = 'exc_1_did_abpm',
        '2' = 'exc_2_complete_abpm',
        '3' = 'exc_3_asleep_1to5',
        '4' = 'exc_4_all_bps'
      ),
      exclusion = recode(
        exclusion,
        '0' = 'All study participants',
        '1' = 'Participants who underwent 24-hour ABPM.',
        '2' = 'Participants with \u22655 asleep systolic and diastolic blood pressure measurements.',
        '3' = 'Participants who were asleep for all measurements between 1am and 5am.',
        '4' = 'Participants with at least 1 systolic and diastolic blood pressure measurement within 30 minutes of all sampling times'
      ),
      .before = 1
    ) %>% 
    mutate(
      Overall = CARDIA + JHS,
      .after = 2
    )
  
  list(data = abpm_long_final, table = exclusion_table)
  
}
