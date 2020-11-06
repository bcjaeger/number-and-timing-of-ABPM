##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param cardia_y30
##' @param jhs_abpm
merge_non_abpm_data <- function(cardia_y30, jhs_vis1) {

  bind_rows(
    CARDIA = cardia_y30,
    JHS = jhs_vis1,
    .id = 'study'
  ) %>% 
    mutate(
      albuminuria = if_else(acr >= 30, 1, 0),
      albuminuria = factor(albuminuria, labels = c("No","Yes")),
      race = fct_collapse(race, 'Black' = c("Black", "Hispanic")),
      edu = fct_collapse(edu, 
        "Less than High School" = c(
          "Elementary",
          "Jr. High",
          "High School",
          "Less than High School"
        ),
        "College graduate" = c(
          "College",
          "Graduate School",
          "College graduate"
        ),
      ),
      edu = fct_relevel(edu, 'College graduate', 'High School graduate/GED')
    ) %>% 
    mutate_if(is.character, as.factor)

}
