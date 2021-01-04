##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param kappa_comparisons
make_figkappa <- function(kappa_comparisons) {

  shorten_lab <- function(lab){
    lab %>% 
      str_replace(' after midnight| after falling asleep', '') %>% 
      str_replace('measurements ', 'measures\n') %>% 
      str_replace('hours', '')
  }
  
  rspec <- round_spec() %>% 
    round_half_even() %>% 
    round_using_decimal(digits = 2)
  
  gg_dat <- kappa_comparisons %>% 
    rename(study_outer = study) %>% 
    unnest(.results) 
  
  
  gg_midnight <- gg_dat %>% 
    filter(str_detect(lab_i, 'midnight'), str_detect(lab_j, 'midnight')) %>% 
    mutate(across(starts_with('lab'), shorten_lab))
    
  
  gg_sleep <- gg_dat %>% 
    filter(str_detect(lab_i, 'sleep'), str_detect(lab_j, 'sleep')) %>% 
    mutate(i = i-1, j = j-1) %>% 
    mutate(across(starts_with('lab'), shorten_lab))
  
  .centers <- list(midnight = gg_midnight, sleep = gg_sleep) %>% 
    map(
      ~ {
        filter(.x, i == j) %>% 
          select(-study_outer) %>% 
          mutate(
            hjust = if_else(study == 'CARDIA', 'right', 'left'),
            vjust = if_else(study == 'CARDIA', 'top', 'bottom'),
            nudge_x = if_else(study == 'CARDIA', +0.1, -0.1),
            nudge_y = if_else(study == 'CARDIA', 0, 0),
            i_strings = if_else(study == 'CARDIA', i-1, i+1) + nudge_x,
            j_strings = if_else(study == 'CARDIA', j+1, j-1) + nudge_y,
            tbv = table_glue("{study}: {100*est}", rspec = rspec)
          )
      }
    )
  
  covers <- function(x, y, point){
    (x < point & y > point) | (x > point & y < point)
  }
  
  .sides <- list(midnight = gg_midnight, sleep = gg_sleep) %>%
    map(
      ~ {
        filter(.x, i != j) %>%
          select(-study, study = study_outer) %>% 
          mutate(
            pval = if_else(
              condition = covers(lwr, upr, 0) & pval < 0.05,
              true = 0.051,
              false = pval
            ),
            tbv = if_else(
              study == 'CARDIA',
              true = table_glue(
                "{-100*est}\n({-100*upr}, {-100*lwr})", 
                rspec = rspec
              ),
              false = table_glue(
                "{100*est}\n({100*lwr}, {100*upr})", 
                rspec = rspec
              )
            ),
            fig_fill = case_when(
              pval < 0.001 ~ 'p < 0.001',
              pval < 0.01  ~ '0.001 \u2264 p < 0.01',
              pval < 0.05  ~ '0.01 \u2264 p < 0.05',
              pval >= 0.05 ~ 'p \u2265 0.05'
            ),
            fig_fill = factor(fig_fill, 
              levels = c(
                'p < 0.001',
                '0.001 \u2264 p < 0.01',
                '0.01 \u2264 p < 0.05',
                'p \u2265 0.05'
              )
            )
          ) 
      }
    )
  
  
  colors_blocks <- paletteer_d("calecopal::desert")
  colors_text <- c('white', 'white', 'white', 'black', 'black')
  #colors_blocks <- paletteer::paletteer_d('nord::frost') %>% c('white')
  
  txt_size <- 2.75
  
  .figs <- map2(.centers, .sides, .f = ~ {
    
    ggplot(.x) + 
      aes(x = j, y = i, label = tbv) + 
      geom_tile(fill = 'white', col = 'white') + 
      geom_text(
        mapping = aes(
          x = j_strings, y = i_strings, 
          hjust = hjust, vjust = vjust
        ),
        size = txt_size,
        fontface = 'bold'
      ) + 
      geom_text(aes(label = lab_i), size = txt_size, fontface = 'italic') + 
      geom_tile(data = .y, aes(fill = fig_fill), col = 'white') + 
      geom_text(
        data = .y, 
        mapping = aes(color = fig_fill), 
        show.legend = FALSE, 
        size = txt_size
      ) + 
      scale_fill_manual(values = rev(colors_blocks)) + 
      scale_color_manual(values = colors_text) + 
      theme_minimal() + 
      labs(
        x = '', 
        y = '', 
        fill = 'Test for difference\nin Kappa statistic'
      ) + 
      theme(
        legend.position = 'top',
        plot.margin = unit(c(1, 3, 1, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 11),
        axis.text = element_blank()
      ) + 
      coord_cartesian(clip = 'off') + 
      annotate("rect", 
        xmin = 0, xmax = 6, 
        ymin = 0, ymax = 6,
        alpha = 0, color = 'black'
      ) +
      annotate("rect",
        xmin = 6, xmax = 12, 
        ymin = 6, ymax = 12,
        alpha = 0, color = 'black'
      ) + 
      annotate(
        geom = 'text', 
        x = 9, 
        y = 12.5, 
        label = 'Distributed blood pressure sampling variants'
      ) + 
      annotate(
        geom = 'text', 
        x = 3, 
        y = -0.5, 
        label = 'Consecutive blood pressure sampling variants'
      ) + 
      scale_y_reverse()
  })
  
  .figs
  
}
