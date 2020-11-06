##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

clean_jhs_abpm <- function() {

  # This cannot be run unless you have access to the shared drive
  # also, you must set the shared drive to Y or change line 12.
  jhs_file_path <- file.path(
    "Y:",
    "REGARDS",
    "JHS",
    "Derived datasets",
    "04-19-2020",
    "data",
    "output"
  )
  
  abpm_path <- file.path(jhs_file_path,"abpm","2020-05-25_abpm_mrp.csv")
  
  jhs_abpm <- read_csv(abpm_path) %>% 
    group_by(subjid) %>% 
    rename(tsm = time_msr) %>% 
    mutate(
      tss = tsm - time_slp,
      tss = case_when(
        status !='Asleep' ~ 0,
        tss < 0 ~ tss + 24,
        TRUE ~ tss
      ),
      asleep_1_to_5 = all(status[tsm >= 1 & tsm <= 5] == "Asleep"),
      nreadings_awk = sum(status=='Awake'),
      nreadings_slp = sum(status=='Asleep')
    ) %>% 
    select(subjid, tsm, tss, sbp, dbp, status,
      asleep_1_to_5, nreadings_slp, sleep_diary_valid)
  
  jhs_abpm
  

}
