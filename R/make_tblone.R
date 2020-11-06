##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
make_tblone <- function(data) {

  tb1_vars <- c(
    'age',
    'sex',
    'race',
    'edu',
    'currentsmoker',
    'diabetes',
    'albuminuria',
    'lvm',
    'lvh',
    'slp_duration',
    'nht_full',
    'bpmeds',
    'slp_sbp_full',
    'slp_dbp_full',
    'cln_sbp',
    'cln_dbp',
    'study'
  )
  
  missings <- data %>% 
    select(all_of(tb1_vars)) %>%
    miss_var_summary() %>% 
    filter(n_miss > 0, variable != 'lvm') %>% 
    mutate(
      variable = recode(
        variable,
        albuminuria = 'albuminuria',
        lvh = 'left ventricular mass and hypertrophy',
        bpmeds = 'antihypertensive medication use',
        currentsmoker = 'Smoking status',
        diabetes = 'diabetes',
        edu = 'education'  
      ),
      tbv = table_glue('{variable}: {n_miss} ({pct_miss}%)')
    )
  
  missing_footer <- as_paragraph(
    paste("Missing counts (%):", paste(missings$tbv, collapse = '; '))
  )
  
  data %>% 
    mutate(
      # b/c tibble_one() is picky about this
      nht_full = recode(nht_full, 'yes' = 'Yes', 'no' = 'No'),
      nht_full = factor(nht_full),
    ) %>% 
    mutate(
      race = factor(race, levels = c('White', 'Black')),
      study = factor(study, levels = c("CARDIA", "JHS"))
    ) %>% 
    set_variable_groups(
      `Blood pressure, mm Hg` = c(
        'slp_sbp_full','slp_dbp_full',
        'cln_sbp','cln_dbp'
      )
    ) %>% 
    set_variable_abbrs(
      lvm = "BSA = body surface area",
      study = c(
        "JHS = Jackson Heart Study",
        "CARDIA = Coronary Artery Risk Development in Young Adults"
      )
    ) %>% 
    set_variable_units(
      age = 'years',
      slp_duration = 'hours',
      lvm = 'g/m2'
    ) %>% 
    set_variable_notes(
      diabetes = "Diabetes was defined as fasting (8+ hours) glucose of at least 126 mg/dL or current use of anti-diabetes medication.",
      currentsmoker = "Smoking status was defined as self-reporting cigarette use within the past year.",
      nht_full = "Nocturnal hypertension was defined as asleep systolic/diastolic blood pressure \u2265120/70 mm Hg."
    ) %>% 
    select(-subjid) %>% 
    select_labelled(
      age	= 'Age',
      sex	= 'Sex',
      race = 'Race',
      edu = 'Education',
      currentsmoker =	'Current smoker',
      diabetes = 'Diabetes',
      albuminuria = 'Albuminuria',
      lvm	= 'Left ventricular mass indexed to BSA',
      lvh = 'Left ventricular hypertrophy',
      slp_duration = 'Sleep duration',
      nht_full = 'Nocturnal hypertension',
      bpmeds = 'Antihypertensive medication use',
      slp_sbp_full =	'Asleep systolic',
      slp_dbp_full	= 'Asleep diastolic',
      cln_sbp =	'Clinic systolic',
      cln_dbp =	'Clinic diastolic',
      study	= 'Study'
    ) %>%
    tibble_one(formula = ~ . | study)

}
