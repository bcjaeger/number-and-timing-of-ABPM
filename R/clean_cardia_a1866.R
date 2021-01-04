##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

clean_cardia_a1866 <- function() {

  # This cannot be run unless you have access to the shared drive
  # also, you must set the shared drive to Y or change line 12.
  
  analysis_file_path <- file.path(
    "Y:", 
    "REGARDS", 
    "CARDIA", 
    "Data", 
    "Population", 
    "Bharat", 
    "A1866-datasets for Byron_December 2020",
    "A1866req12_23_2002"
  )
  
  # Read summary abpm data from J. Schwartz ----------------------------------
  
  # not using it, but left this section here in case of update.
  
  # Read data request from J. Bundy ------------------------------------------
  
  cardia_all <- read_sas(
    file.path(analysis_file_path, 'a1866req12_23_2020.sas7bdat')
  ) %>% 
    set_names(tolower(names(.))) 
  
  cardia_merged <- cardia_all %>% 
    select(
      sid = short_id,
      age = a01age2,
      sex = a01sex,
      race = a01race1,
      educ = a01ed1,
      smoke = i10smoke,
      height_cm = i20hgt,
      weight_lbs = i20wgt,
      bpmeds = i02bmedam,
      trig = il1ntrig,
      sbp = i02csavg,
      dbp = i02cdavg,
      acr = il1ratio,
      diab = i08diab,
      phys = i18total,
      pulse = i02pulse,
      # echo variables
      pwt = ilvpwd2d,
      lvedd = ilvidd2d,
      lvmass2d = ilvmass2d,
      center
    ) %>% 
    mutate(
      sex = factor(
        sex,
        levels = c(1, 2),
        labels = c("Male", "Female")
      ),
      race = factor(
        race,
        levels = c(3, 4, 5),
        labels = c("Hispanic", "Black", "White")
      ),
      center = factor(
        center,
        levels = 1:4,
        labels = c('BHAM', 'CHIC', 'MINN', 'OAKL')
      ),
      currentsmoker = recode(
        smoke,
        '0' = 'No',
        '1' = 'No',
        '2' = 'Yes'
      ),
      diab = recode(
        diab,
        '1' = 'No', 
        '2' = 'Yes',
        '8' = NA_character_
      ),
      # make pulse in beats per minute
      pulse = pulse * 2,
      educ = cut(
        educ,
        breaks = c(0, 6, 8, 12, 16, 98),
        labels = c(
          "Elementary",
          "Jr. High",
          "High School",
          "College",
          "Graduate School"
        )
      ),
      bpmeds = case_when(
        bpmeds == 9 ~ "No",
        bpmeds %in% c(1, 2) ~ "Yes"
      ),
      # right wall thickness
      rwt = 2 * pwt / lvedd,
      # left ventricular mass indexed by height
      lvm_ht = lvmass2d / ((height_cm/100)^2.7)
    )
  
  
  cardia_merged$educ[cardia_merged$educ == 99] = NA
  
  
  # Merge analysis data with echo data and select analysis columns ----------
  
  cardia_munged <- cardia_merged %>%
    mutate(
      weight_kg = weight_lbs * 0.453592,
      bsa = sqrt( (height_cm * weight_kg ) / 3600 ),
      lvm_bsa = lvmass2d/(bsa),
      lvh_ht = if_else( 
        (sex=='Male' & lvm_ht > 48) | (sex=='Female' & lvm_ht > 44), 
        true = 'Yes', false = 'No'
      ),
      lvh_bsa = if_else( 
        (sex=='Male' & lvm_bsa > 115) | (sex=='Female' & lvm_bsa > 95),
        true = 'Yes', false = 'No'
      ),
      sid = as.character(sid),
      age = age + 30
    ) %>% 
    select(
      subjid = sid,
      age,
      sex,
      race,
      edu = educ,
      diabetes=diab,
      currentsmoker,
      bpmeds = bpmeds,
      cln_sbp = sbp,
      cln_dbp = dbp,
      lvm = lvm_bsa,
      lvh = lvh_bsa,
      acr
    )
  
  # drop rows missing all the data (these participants did not attend y30)
  
  cardia_year30 <- cardia_munged %>% 
    filter(
      !(is.na(diabetes) &
          is.na(currentsmoker) &
          is.na(bpmeds) &
          is.na(cln_sbp) &
          is.na(cln_dbp) &
          is.na(lvm) &
          is.na(lvh) &
          is.na(acr) )
    )
  
  cardia_year30

}
