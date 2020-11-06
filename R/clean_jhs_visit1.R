##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

clean_jhs_visit1 <- function() {

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
  
  vis1_path <- file.path(jhs_file_path,"visit1.csv")
  
  jhs_vis1 <- read_csv(vis1_path) %>% 
    select(
      subjid,
      age,
      sex,
      edu = edu3cat,
      diabetes,
      currentsmoker,
      bpmeds,
      cln_sbp = sbp,
      cln_dbp = dbp,
      lvm,
      lvh,
      acr
    ) %>% 
    mutate(
      race = 'Black',
      edu = factor(edu,
        levels = c(
          "Less than high school",
          "High school graduate/GED",
          "Attended vocational school, trade school, or college"
        ),
        labels = c(
          "Less than High School",
          "High School graduate/GED",
          "College graduate"
        ) 
      )
    )
  

}
