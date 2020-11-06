##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

clean_cardia_y30 <- function() {

  # This cannot be run unless you have access to the shared drive
  # also, you must set the shared drive to Y or change line 12.
  analysis_file_path <- file.path(
    "Y:",
    "Users",
    "Jaeger",
    "Datasets",
    "CARDIA_analysis",
    "source"
  )
  
  # Read summary abpm data from J. Schwartz ----------------------------------
  
  # not using it, but left this section here in case of update.
  
  # Read data request from J. Bundy ------------------------------------------
  
  cardia_all <- read_sas(
    file.path(analysis_file_path, 'a1850req10_16_2018.sas7bdat')
  ) %>% 
    set_names(tolower(names(.))) %>% 
    # these variables are in the df_add set
    select(-ends_with("total"))
  
  cardia_all_acr <- read_sas(
    file.path(analysis_file_path, 'a1850req11_1_2018.sas7bdat')
  ) %>% 
    set_names(tolower(names(.)))
  
  # Read data request from N. Bello -----------------------------------------
  
  cardia_echo <- read_sas(
    file.path(analysis_file_path, "a1777req11_16_2017.sas7bdat")
  ) %>% 
    set_names(tolower(names(.))) %>%
    mutate(
      bpmeds = case_when(
        i02bmedam == 9 ~ "No",
        i02bmedam %in% c(1, 2) ~ "Yes"
      )
    ) %>% 
    select(
      sid          = short_id,
      bpmeds       = bpmeds,
      height_cm    = i20hgt,
      weight_lbs   = i20wgt,
      # echo variables
      pwt          = ilvpwd2d,
      lvedd        = ilvidd2d,
      lvmass2d     = ilvmass2d
    ) %>% 
    mutate(
      # right wall thickness
      rwt    = 2 * pwt / lvedd,
      # left ventricular mass indexed by height
      lvm_ht = lvmass2d / ((height_cm/100)^2.7)
    )
  
  # Merge ACR and collect analysis columns ----------------------------------
  
  cardia_merged <- cardia_all %>%
    left_join(cardia_all_acr) %>%
    select(
      sid = short_id,
      center,
      age = a01age2,
      sex = a01sex,
      race = a01race1,
      educ = a01ed1,
      ends_with("avgsy"),
      ends_with("hbnow"),
      ends_with("csavg"),
      ends_with("avgdi"),
      ends_with("cdavg"),
      ends_with("bmi"),
      ends_with("l1chol"),
      ends_with("l1hdl"),
      ends_with("l1ldl"),
      ends_with("ratio"),
      ends_with("l1ntrig"),
      ends_with("10smoke"),
      ends_with("pulse"),
      ends_with("cglu"),
      ends_with("total"),
      ends_with("08diab")
    )
  
  
  # Munge the data ----------------------------------------------------------
  
  # some variable names have a number pattern
  # for example, (a-i)02avgsy. the '02' is a number pattern.
  # this code replaces number patterns with the symbol '_'
  # so that the columns can be handled systematically
  
  patterns = c("02", "20", "l1", "10", "l7", "08", "l8", "18")
  
  for (p in patterns)
    names(cardia_merged) = gsub(p, "_", names(cardia_merged))
  
  # education is set to 99 when it is missing
  # this code sets the missing values of educ to NA
  cardia_merged$educ[cardia_merged$educ == 99] = NA
  
  cardia_munged <- cardia_merged %>% 
    mutate(
      # categorical variables 
      # change numeric values to factors
      sex = factor(sex,
        levels = c(1, 2),
        labels = c("Male", "Female")),
      race = factor(race,
        levels = c(3, 4, 5),
        labels = c("Hispanic", "Black", "White")),
      center = factor(center,
        levels = 1:4,
        labels = c('BHAM', 'CHIC', 'MINN', 'OAKL')),
      educ = cut(educ,
        breaks = c(0, 6, 8, 12, 16, 98),
        labels = c(
          "Elementary",
          "Jr. High",
          "High School",
          "College",
          "Graduate School"
        )
      )
    ) %>% 
    pivot_longer(
      cols = -c(sid, center, age, sex, race, educ),
      names_to = 'variable',
      values_to = 'value'
    ) %>%
    mutate(
      # Change variable names
      variable = as.character(variable),
      variable = gsub("ntrig", "trig", variable),
      variable = gsub("csavg", "sbp", variable),
      variable = gsub("avgsy", "sbp", variable),
      variable = gsub("cdavg", "dbp", variable),
      variable = gsub("avgdi", "dbp", variable),
      variable = gsub("ratio", "acr", variable),
      variable = gsub("total", "phys", variable)
    ) %>%
    # separate variable column into exam and variable column
    # this works because CARDIA uses a-i to as a prefix
    # for column names to indicate the exam year.
    separate(col = 'variable', into = c("exam", "variable")) %>%
    mutate(
      exam = recode(
        exam, 
        'c' = '5',
        'd' = '7',
        'e' = '10',
        'f' = '15',
        'g' = '20',
        'h' = '25',
        'i' = '30'
      ),
      exam = as.numeric(exam)
    ) %>%
    # spread the columns back out
    pivot_wider(names_from = variable, values_from = value) %>% 
    mutate(
      currentsmoker = recode(smoke,
        '0' = 'No',
        '1' = 'No',
        '2' = 'Yes'
      ),
      diab = recode(diab,
        '1' = 'No', 
        '2' = 'Yes',
        '8' = NA_character_
      ),
      # make pulse in beats per minute
      pulse = pulse * 2
    ) %>% 
    select(-smoke)
  
  
  # Merge analysis data with echo data and select analysis columns ----------
  
  cardia_year30 <- cardia_munged %>%
    filter(exam == 30) %>% 
    left_join(cardia_echo, by = 'sid') %>% 
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
  
  cardia_year30 <- cardia_year30 %>% 
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

}
