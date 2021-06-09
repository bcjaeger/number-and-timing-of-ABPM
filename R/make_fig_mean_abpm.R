#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param abpm_long
make_fig_mean_abpm <- function(abpm_long, abpm_wide) {

  gg_data <- bind_rows(
    CARDIA = make_data_for_fig_mean_abpm(abpm_long, 'CARDIA'),
    JHS = make_data_for_fig_mean_abpm(abpm_long, 'JHS'),
    .id = 'study'
  )
  
  abpm_means <- abpm_wide %>% 
    group_by(study) %>% 
    summarize(sbp = mean(slp_sbp_full),
              dbp = mean(slp_dbp_full))
  
  plot_sbp <- ggplot(gg_data) + 
    aes(time, prd_sbp, ymin = lwr_sbp, ymax = upr_sbp) + 
    labs(y = 'Systolic blood pressure, mm Hg',
         x = 'Time, hours') + 
    geom_hline(data = abpm_means,
               aes(yintercept = sbp),
               linetype = 2, 
               alpha = 0.3)
  
  plot_dbp <- ggplot(gg_data) + 
    aes(time, prd_dbp, ymin = lwr_dbp, ymax = upr_dbp) + 
    labs(y = 'Diastolic blood pressure, mm Hg',
         x = 'Time, hours') + 
    geom_hline(data = abpm_means,
               aes(yintercept = dbp),
               linetype = 2, 
               alpha = 0.3) 
  
  list(sbp = plot_sbp,
       dbp = plot_dbp) %>% 
    map(
      ~ .x + 
        geom_line() + 
        geom_ribbon(alpha = 0.15) +
        theme_bw() +
        facet_grid(study~time_cat, switch = 'x', scales = 'free_y') +
        theme(panel.grid = element_blank()) + 
        scale_x_continuous(breaks = 0:7) 
    )
      



}

make_data_for_fig_mean_abpm <- function(abpm_long, .study){
  
  data_tsm <- abpm_long %>% 
    filter(status == 'Asleep',
           study == .study, 
           tsm < 7) %>% 
    rename(time = tsm) %>% 
    mutate(time_cat = 'tsm')
  
  data_tss <- abpm_long %>% 
    filter(status == 'Asleep',
           study == .study, 
           tss < 7) %>% 
    rename(time = tss) %>% 
    mutate(time_cat = 'tss')
  
  list(
    tss = data_tss, 
    tsm = data_tsm
  ) %>% 
    map_dfr(
      ~ {
        
        m_sbp <- lm(sbp ~ bs(time), data = .x)
        m_dbp <- lm(dbp ~ bs(time), data = .x)
        d <- tibble(time = seq(0.2, 6.8, length.out = 1000))
        
        prd_sbp <- predict(m_sbp, newdata = d, se.fit = TRUE)
        prd_dbp <- predict(m_dbp, newdata = d, se.fit = TRUE)
        
        d$prd_sbp <- prd_sbp$fit
        d$lwr_sbp <- prd_sbp$fit + qnorm(p = 0.025) * prd_sbp$se.fit
        d$upr_sbp <- prd_sbp$fit + qnorm(p = 0.975) * prd_sbp$se.fit
        
        d$prd_dbp <- prd_dbp$fit
        d$lwr_dbp <- prd_dbp$fit + qnorm(p = 0.025) * prd_dbp$se.fit
        d$upr_dbp <- prd_dbp$fit + qnorm(p = 0.975) * prd_dbp$se.fit
        
        d
        
      },
      .id = 'time_cat'
    ) %>% 
    mutate(
      time_cat = recode(
        time_cat, 
        'tsm' = 'Time since midnight',
        'tss' = 'Time since falling asleep'
      )
    )
  
}