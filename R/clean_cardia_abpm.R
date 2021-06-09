##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

clean_cardia_abpm <- function() {

  # This cannot be run unless you have access to the shared drive
  # also, you must set the shared drive to Y or change line 15.
  
  cardia_abpm <- 
    read_sas(
      file.path(
        "Y:",
        "REGARDS",
        "CARDIA",
        "Data",
        "Population",
        "Bharat",
        "A1866-datasets for Byron_December 2020",
        "Y30ABPM_analysisfiles",
        "spacelabs_abp2_edited.sas7bdat"
      )
    ) %>% 
    set_names(tolower(names(.))) %>% 
    filter(period %in% c("AW","SL"), valid_reading == 1) %>% 
    mutate(tim = (tim / (60^2)) %% 24) %>% 
    rename(tsm = tim, 
           subjid = id,
           sbp = sysbp,
           dbp = diabp) %>% 
    group_by(subjid) %>% 
    mutate(
      period = as.character(period),
      nreadings_slp = sum(period == "SL"),
      nreadings_awk  = sum(period == "AW"),
      phase_awk = mark_period(period, 'AW'),
      phase_slp = mark_period(period, 'SL'),
      sleep_diary_valid = 1
    ) %>%
    filter(phase_awk < 3, phase_slp < 2) %>% 
    select(-starts_with('phase')) %>% 
    mutate(asleep_1_to_5 = all(period[tsm>=1 & tsm<=5]=="SL")) %>% 
    group_by(subjid, period) %>% 
    mutate(tss = tsm - tsm[1]) %>% 
    ungroup() %>% 
    mutate(
      tss = case_when(
        period == 'AW' ~ 0,
        tss < 0 ~ tss + 24,
        TRUE ~ tss
      ),
      status = recode(period,
        'AW' = 'Awake',
        'SL' = 'Asleep'),
      subjid = as.character(subjid)
    ) %>% 
    select(
      -period,
      -site,
      -reading,
      -hr,
      -nreadings_awk
    )
  
  cardia_abpm

}
