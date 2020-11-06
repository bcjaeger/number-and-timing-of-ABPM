##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param tbl_exclusions
##' @param tbl_characteristics
##' @param tbl_accuracy
##' @param tbl_comparison
##' @param tbl_prs
##' @param control_vars
##' @param fig_errors
make_report <- function(tbl_exclusions = exclusions$table, tbl_characteristics,
  tbl_variations, tbl_accuracy, tbl_cstats, tbl_prs, control_vars, fig_errors,
  fig_kappa_comparison) {
  
  # Setup -------------------------------------------------------------------
  
  tbls_main <- tbls_supp <- tibble(
    object = list(), 
    caption = NA_character_
  )
  
  figs_main <- figs_supp <- tibble(
    object = list(),
    caption = character(),
    legend = character(),
    width = numeric(),
    height = numeric()
  )
  
  abbrevs <- c(
    CARDIA = 'Coronary Artery Risk Development in Young Adults',
    CI = 'confidence interval',
    BSA = "body surface area",
    BP = 'blood pressure',
    GED = 'General Educational Development',
    JHS = 'Jackson Heart Study',
    ABPM = 'ambulatory blood pressure monitoring',
    BSA = 'body surface area'
  )
  
  fts <- c(
    '\u2A',
    '\u2020',
    '\u2021',
    '\uA7',
    '\u2016',
    '\uB6',
    '#',
    '\u2a\u2a',
    '\u2020\u2020',
    '\u2021\u2021',
    '\u2021\u2021\u2021\u2021'
  )
  
  lab_acc <- "accuracy of different sampling strategies"
  lab_est <- "estimate mean blood pressure during sleep"
  lab_bps <- "blood pressure sampling times"
  lab_cda <- "Coronary Artery Risk Development in Young Adults study"
  lab_cnc <- "consecutive strategy"
  lab_dst <- "distributed strategy"
  lab_jhs <- "Jackson Heart Study"
  lab_msr <- "2-4 measurements"
  lab_prt <- "participants in the"
  lab_wnr <- "the highest kappa statistic"
  lab_pr  <- 'prevalence ratio'
  lab_ci  <- '95% confidence interval'
  lab_nbp <- 'blood pressure during sleep'
  lab_lvh <- 'left ventricular hypertrophy'
  lab_alb <- 'albuminuria'
  lab_abp <- 'ambulatory blood pressure monitoring'
  
  footnote_winners <- as_paragraph(
    'Blood pressure sampling variations were compared to other ',
    'variations that measure blood pressure the same number of times ',
    '(i.e., 2, 3, or 4) using the same strategy (i.e., Consecutive ', 
    'or distributed) and the same time reference (i.e., midnight or ',
    'onset of sleep). Each of these 12 comparison groups had one ',
    'variation with the highest overall Kappa statistic, ',
    'and those variations are presented here.'
  )
  
  footnote_kappa_defn <- as_paragraph(
    'Kappa statistics measure the chance-corrected agreement in ',
    'classification of nocturnal hypertension between ',
    'ambulatory blood pressure monitoring throughout sleep and ', 
    'a blood pressure sampling variation.'
  )
  
  .tsm <- 'Time since midnight, hours'
  .tss <- 'Time since falling asleep, hours'
  
  .cda <- 'CARDIA participants'
  .jhs <- 'JHS participants'
  
  .cnc <- 'consecutive strategy'
  .dst <- 'distributed strategy'
  
  .cap <- function (string) {
    capped <- grep("^[A-Z]", string, invert = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
    return(string)
  }
  
  # Exclusions --------------------------------------------------------------
  
  .tbl_exclusions <- tbl_exclusions %>% 
    mutate(across(c(CARDIA, JHS), ~tbl_val(as.integer(.x)))) %>% 
    flextable(theme_fun = theme_box) %>% 
    set_header_labels(
      exclusion = 'Inclusion criteria',
      CARDIA = 'CARDIA participants',
      JHS = 'JHS participants'
    ) %>% 
    width(j=1, width=2.5) %>% 
    width(j=2:3, width = 2.2) %>% 
    footnote(i = 1, j = 1, ref_symbols = '', value = as_paragraph(
      write_abbrevs(abbrevs[c('CARDIA', 'JHS', 'ABPM')])
    ))
  
  tbls_supp %<>% add_row(
    object = list(.tbl_exclusions), 
    caption = glue("Participant inclusion cascade.")
  ) 
  

  # Characteristics ---------------------------------------------------------
  
  .tbl_characteristics <- tbl_characteristics %>% 
    footnote(i = 1, j = 1, ref_symbols = '', value = as_paragraph(
        write_abbrevs(abbrevs[c('CARDIA', 'BSA', 'GED', 'JHS')])))
  
  tbls_main %<>% add_row(
    object = list(.tbl_characteristics), 
    caption = "Participant characteristics overall and stratified by study."
  ) 
  

  # Summary of BP samplers --------------------------------------------------

  .tbl_variations <- tbl_variations %>% 
    footnote(i= 1, j=1, ref_symbols = '', 
      value = as_paragraph(write_abbrevs(abbrevs[c('BP')])))
  
  tbls_supp %<>% add_row(
    object = list(.tbl_variations),
    caption = "Summary of 12 groups of blood pressure sampling variations"
  )

  # Accuracy of BP samplers -------------------------------------------------
  
  .tbl_accuracy <- map(
    .x = tbl_accuracy,
    .f = ~ .x %>% 
      bg(i = ~ !is.na(n_msr), bg = 'grey80') %>% 
      italic(i = ~ !is.na(n_msr), italic = TRUE) %>% 
      height(height = 1.5, part = 'header') %>% 
      footnote(i = 1, j = 1, ref_symbols = '', value = as_paragraph(
        write_abbrevs(abbrevs[c('CARDIA', 'BP', 'JHS')])))
  )
  
  .tbl_accuracy$winners %<>% 
    footnote(i = 1:2, j = 1, part = 'header', ref_symbols = fts[1],
      value = footnote_winners) %>% 
    footnote(i = 1, j = 2, part = 'header', ref_symbols = fts[2], 
      value = footnote_kappa_defn)
  
  .tbl_accuracy$everyone %<>% 
    footnote(i = 1, j = 2, part = 'header', ref_symbols = fts[1], 
      value = footnote_kappa_defn)
  
  tbls_main %<>% add_row(
    object = list(.tbl_accuracy$winners), 
    caption = "summary of 12 blood pressure sampling variations that obtained the highest overall chance-corrected agreement (i.e., Kappa statistic) with ambulatory blood pressure monitoring throughout sleep."
  ) 
  
  tbls_supp %<>% add_row(
    object = list(.tbl_accuracy$everyone), 
    caption = "Summary of all 74 blood pressure sampling variations that were evaluated in the current study."
  ) 
  

  # Prevalence ratios using BP Samplers --------------------------------------
  
  control_labs <- recode(
    control_vars,
    "age" = "participant age",
    "sex" = "sex",
    "diabetes" = "diabetes status",
    "currentsmoker" = "smoking status",
    "bpmeds" = "antihypertensive medication use",
    "slp_duration" = "sleep duration",
    "cln_sbp" = "systolic blood pressure in the clinical setting",
    "cln_dbp" = "diastolic blood pressure in the clinical setting"
  )
  
  control_footer <- paste("Prevalence ratios are adjusted for",
    glue_collapse(control_labs, sep = ', ', last = ' and ')) %>% 
    as_paragraph()
  
  .tbl_prs <- tbl_prs %>% 
    map(
      ~ .x %>% 
        bg(i = ~ !is.na(n_msr), bg = 'grey80') %>% 
        italic(i = ~ !is.na(n_msr), italic = TRUE) %>% 
        height(height = 1.5, part = 'header') %>% 
        footnote(i = 1, j = 1, ref_symbols = '', value = as_paragraph(
          write_abbrevs(abbrevs[c('CARDIA', 'JHS')])))
    )
  
  .tbl_prs$lvh_sbp_winners %<>% 
    footnote(i=1, j=1, ref_symbols=fts[1], 
      value=footnote_winners, part = 'header') %>% 
    footnote(i=2, j=c(2,4,6), ref_symbols=fts[2], 
      value=control_footer, part = 'header') %>% 
    footnote(i=2, j=c(2,4,6), ref_symbols=fts[3], part = 'header', 
      value=as_paragraph(
        "Prevalence ratios correspond to 10 mm Hg higher ",
        "systolic blood pressure"
      )
    )
  
  .tbl_prs$alb_sbp_winners %<>% 
    footnote(i=1, j=1, ref_symbols=fts[1], 
      value=footnote_winners, part = 'header') %>% 
    footnote(i=2, j=c(2,4,6), ref_symbols=fts[2], 
      value=control_footer, part = 'header') %>% 
    footnote(i=2, j=c(2,4,6), ref_symbols=fts[3], part = 'header', 
      value=as_paragraph(
        "Prevalence ratios correspond to 10 mm Hg higher ",
        "systolic blood pressure"
      )
    )
  
  
  tbls_main %<>% add_row(
    object = list(.tbl_prs$lvh_sbp_winners), 
    caption = "Prevalence ratios (95% confidence intervals) for the association between mean systolic blood pressure during sleep and left ventricular hypertrophy using the 12 blood pressure sampling strategies that obtained the highest overall chance-corrected agreement (i.e., Kappa statistic) with measuring blood pressure throughout sleep.")
  
  tbls_supp %<>% add_row(
    object = list(.tbl_prs$alb_sbp_winners), 
    caption = "Prevalence ratios (95% confidence intervals) for the association between mean systolic blood pressure during sleep and albuminuria using the 12 blood pressure sampling strategies that obtained the highest overall chance-corrected agreement (i.e., Kappa statistic) with measuring blood pressure throughout sleep.")
  
  # C-statistics of BP samplers ---------------------------------------------

  .tbl_cstats <- map(
    .x = tbl_cstats,
    .f = ~ .x %>% 
      bg(i = ~ !is.na(n_msr), bg = 'grey80') %>% 
      italic(i = ~ !is.na(n_msr), italic = TRUE) %>% 
      height(height = 1.5, part = 'header') %>% 
      footnote(i = 1, j = 1, ref_symbols = '', value = as_paragraph(
        write_abbrevs(abbrevs[c('CARDIA', 'BP', 'CI', 'JHS')])))  
  )
  
  .tbl_cstats[c('lvh_winners', 'alb_winners')] %<>% map(
      ~ footnote(.x,
        i = 2, j = 1, part = 'header', ref_symbols = fts[1],
        value = footnote_winners
      ) %>% 
        footnote(
          i = 2, j = 2, part = 'header', ref_symbols = fts[2],
          value = as_paragraph(
            'Overall concordance was defined as the concordance statistic ',
            'resulting from concatenating predicted probabilities and observed ',
            'status across the two cohorts and two outcome variables.'
          )
        ) %>% 
        footnote(
          i = 2, j = 2, part = 'header', ref_symbols = fts[3],
          value = as_paragraph(
            'All concordance statistics obtained from blood pressure ',
            'sampling variations were compared to ',
            'the concordance statistic obtained when blood pressure was ',
            'measured throughout sleep.'
          )
        ) %>% 
        footnote(
          i = 2, j = 3, part = 'header', ref_symbols = fts[4],
          value = as_paragraph('P-values were obtained using ',
            'DeLong\'s test for correlated concordance statistics.')
        )
    )

  
  .tbl_cstats[c('lvh', 'alb')] %<>% map(
    ~ footnote(.x, 
      i = 2, j = 2, part = 'header', ref_symbols = fts[1],
      value = as_paragraph(
        'Overall concordance was defined as the concordance statistic ',
        'resulting from concatenating predicted probabilities and observed ',
        'status across the two cohorts and two outcome variables.'
      )
    ) %>% 
      footnote(
        i = 2, j = 2, part = 'header', ref_symbols = fts[2],
        value = as_paragraph(
          'All concordance statistics obtained from blood pressure ',
          'sampling variations were compared to ',
          'the concordance statistic obtained when blood pressure was ',
          'measured throughout sleep.'
        )
      ) %>% 
      footnote(
        i = 2, j = 3, part = 'header', ref_symbols = fts[4],
        value = as_paragraph('P-values were obtained using ',
          'DeLong\'s test for correlated concordance statistics.')
      )
  )
    
  tbls_main %<>% add_row(
    object = list(.tbl_cstats$lvh_winners), 
    caption = "Concordance statistics for left-ventricular hypertrophy based on models using the 12 blood pressure sampling strategies, separately, that obtained the highest overall chance-corrected agreement with ambulatory blood pressure monitoring throughout sleep.") 
  
  tbls_supp %<>% add_row(
    object = list(.tbl_cstats$alb_winners), 
    caption = "Concordance statistics for albuminuria based on models using the 12 blood pressure sampling strategies, separately, that obtained the highest overall chance-corrected agreement with ambulatory blood pressure monitoring throughout sleep.")
  
  # Figure: BP sampling diagram ---------------------------------------------
  
  emf(
    file = "doc/bp_sample_diagram.emf", 
    width = 6, 
    height = 7,
    coordDPI = 600
  )
  
  par(mar = c(4, 1, 1, 1))
  openplotmat()
  
  xcoords <- c(1:5) / 6
  
  scoords <- list(
    c(1, 2, 4, 5) / 6,
    c(1, 2, 3) / 6,
    c(2, 4) / 6
  )
  
  ycoords <- c(0.10, 0.25, 0.40)
  
  textempty(mid = c(0.50, 0.50), lab = 'Distributed strategy examples')
  
  axis(
    side = 1,
    at = xcoords,
    labels = paste0(1:5, ":00 AM")
  )
  
  textempty(mid = c(xcoords[1] / 2, ycoords[1]), lab = 'Four BP \nreadings')
  textempty(mid = c(xcoords[1] / 2, ycoords[2]), lab = 'Three BP \nreadings')
  textempty(mid = c(xcoords[1] / 2, ycoords[3]), lab = 'Two BP \nreadings')
  
  for(j in 1:length(scoords)){
    
    ss <- scoords[[j]]
    yy <- ycoords[j]
    
    for (i in 1:length(ss)) {
      textrect(
        c(ss[i], yy),
        radx = 0.01,
        rady = 0.01,
        box.col = "black",
        shadow.size = 1e-10,
        cex = 1.5
      )
    }
    
  }
  
  for(i in 1:length(ycoords)){
    lines(c(xcoords[1], xcoords[5]), c(ycoords[i], ycoords[i]),lty=2)
  }
  
  textempty(mid = c(0.50, 0.99), lab = 'Consecutive strategy examples')
  
  xcoords <- seq(1,5,length.out = 10)/6
  
  scoords <- list(
    c(1,1.5,2,2.5)/6,
    c(2,2.5,3)/6,
    c(2,2.5)/6
  )
  
  ycoords <- c(0.60, 0.75, 0.90)
  
  textempty(mid = c(xcoords[1] / 2, ycoords[1]), lab = 'Four BP \nreadings')
  textempty(mid = c(xcoords[1] / 2, ycoords[2]), lab = 'Three BP \nreadings')
  textempty(mid = c(xcoords[1] / 2, ycoords[3]), lab = 'Two BP \nreadings')
  
  for(j in 1:length(scoords)){
    
    ss <- scoords[[j]]
    yy <- ycoords[j]
    
    for (i in 1:length(ss)) {
      textrect(
        c(ss[i], yy),
        radx = 0.01,
        rady = 0.01,
        box.col = "black",
        shadow.size = 1e-10,
        cex = 1.5
      )
    }
    
  }
  
  for(i in 1:length(ycoords)){
    lines(c(xcoords[1], xcoords[10]), c(ycoords[i], ycoords[i]),lty=2)
  }
  
  dev.off()
  
  figs_main %<>% 
    add_row(
      object  = list('doc/bp_sample_diagram.emf'),
      caption = glue(
        'Illustration of blood pressure sampling variations',
        ' following a consecutive and distributed sampling strategy.'
      ),
      legend  = '',
      width   = 6,
      height  = 7
    )
  
  # Kappa comparisons -------------------------------------------------------
  
  figs_main %<>% add_row(
    object  = list(fig_kappa_comparison$sleep),
    caption = glue(
      "Summary of Kappa statistics (multiplied by 100)",
      " for the 6 blood pressure",
      " sampling variations with highest overall Kappa statistics among",
      " those that measured time in hours since falling asleep.",
      " Panels on the diagonal (white background) show the Kappa statistic",
      " values for participants in the JHS (lower left)",
      " and CARDIA study (upper right).",
      " Panels on the off-diagonal show bootstrapped differences between", 
      " the Kappa statistics presented on the corresponding diagonal tiles;",
      " differences between the JHS Kappa statistics are shown below the",
      " diagonal while differences between the CARDIA Kappa statistics are",
      " shown above the diagonal."
    ),
    legend  = glue(
      "Confidence intervals were estimated using bootstrap resampling",
      " with bias correction and acceleration. Each interval was based",
      " on the aggregate of 10,000 bootstrap replicates."),
    width   = 7.1,
    height  = 6
  )
  
  
  figs_supp %<>% add_row(
    object  = list(fig_kappa_comparison$midnight),
    caption = glue(
      "Summary of Kappa statistics (multiplied by 100)",
      " for the 6 blood pressure",
      " sampling variations with highest overall Kappa statistics among",
      " those that measured time in hours since midnight.",
      " Panels on the diagonal (white background) show the Kappa statistic",
      " values for participants in the JHS (lower left)",
      " and CARDIA study (upper right).",
      " Panels on the off-diagonal show bootstrapped differences between", 
      " the Kappa statistics presented on the corresponding diagonal tiles;",
      " differences between the JHS Kappa statistics are shown below the",
      " diagonal while differences between the CARDIA Kappa statistics are",
      " shown above the diagonal."
    ),
    legend  = c(
      glue("Confidence intervals were estimated using bootstrap resampling",
           " with bias correction and acceleration. Each interval was based",
           " on the aggregate of 10,000 bootstrap replicates.")
    ),
    width   = 7.1,
    height  = 6
  )
  
  
  
  # Format tables for manuscript ---------------------------------------------
  
  font_size = 11
  font_name = "Calibri"
  
  ntbl_main <- nrow(tbls_main)
  nfig_main <- nrow(figs_main)
  ntbl_supp <- nrow(tbls_supp)
  nfig_supp <- nrow(figs_supp)

  if(ntbl_main > 0){
    tbls_main %<>% 
      mutate(
        pre_cap = glue("Table {seq(ntbl_main)}"),
        caption = glue("{pre_cap}: {caption}"),
        object = map(
          .x = object, 
          .f = ~ .x %>% 
            font(fontname = font_name, part = 'all') %>% 
            fontsize(size = font_size, part = 'all') %>% 
            height(height = 2, part = 'footer')
        )
      ) %>% 
      select(-pre_cap)
  }
  
  if(ntbl_supp > 0){
    tbls_supp %<>% 
      mutate(
        pre_cap = glue("Table S{seq(ntbl_supp)}"),
        caption = glue("{pre_cap}: {caption}"),
        object = map(
          .x = object, 
          .f = ~ .x %>% 
            font(fontname = font_name, part = 'all') %>% 
            fontsize(size = font_size, part = 'all') %>% 
            height(height = 2, part = 'footer')
        )
      ) %>% 
      select(-pre_cap)
  }
  
  if(nfig_main > 0){
    figs_main %<>% 
      mutate(
        pre_cap = glue("Figure {seq(nfig_main)}"),
        caption = glue("{pre_cap}: {caption}")
      ) %>% 
      select(-pre_cap)
  }  
  
  if(nfig_supp > 0){
    figs_supp %<>% 
      mutate(
        pre_cap = glue("Figure S{seq(nfig_supp)}"),
        caption = glue("{pre_cap}: {caption}")
      ) %>% 
      select(-pre_cap)
  }
  
  # Send output to word doc -------------------------------------------------
  
  my_doc <- read_docx('doc/template.docx') %>% 
    #cursor_reach(keyword = 'MAIN') %>% 
    body_add_break()
  
  if(ntbl_main > 0){
    for(i in seq(ntbl_main)){
      
      if(i == 1){
        my_doc %<>% 
          body_add_par(tbls_main$caption[[i]]) %>% 
          body_add_flextable(tbls_main$object[[i]]) %>% 
          #body_add_break() %>% 
          body_end_section_continuous() 
      }
      
      if(i == 2){
        my_doc %<>% 
          body_add_par(tbls_main$caption[[i]]) %>% 
          body_add_flextable(tbls_main$object[[i]]) %>% 
          body_add_break() %>% 
          body_end_section_landscape()
      }
      
      if(i > 2){
        my_doc %<>% 
          body_add_par(tbls_main$caption[[i]]) %>% 
          body_add_flextable(tbls_main$object[[i]]) %>% 
          body_add_break() 
      }
      
      

    }
  }
  
  my_doc %<>% body_end_section_portrait()
  
  # Main figures
  
  if(nfig_main > 0){
    
    for(i in seq(nfig_main)){
      
      if( inherits(figs_main$object[[i]], 'gg') ) {
        
        filename <- tempfile(fileext = ".emf")
        
        emf(
          file = filename, 
          width = figs_main$width[i], 
          height = figs_main$height[i],
          coordDPI = 600
        )
        
        print(figs_main$object[[i]])
        
        dev.off()
        
      } else {
        
        # This assumes the above has been taken care of already
        filename <- figs_main$object[[i]]
        
      }
      
      my_doc %<>% 
        body_add_par(figs_main$caption[i]) %>% 
        body_add_img(
          filename, 
          width = figs_main$width[i],
          height = figs_main$height[i]
        )
      
      if( length(figs_main$legend[i])>0 ){
        
        my_doc %<>% body_add_par(figs_main$legend[i])
        
      }
      
      if(i == nfig_main) my_doc %<>% body_add_break()
      
    }
  }
  
  
  # 
  # Supplemental tables
  # my_doc %<>% 
    # cursor_reach(keyword = 'SUPPLEMENT') %>% 
    # body_add_break()
  
  if(ntbl_supp > 0){
    
    for(i in seq(ntbl_supp)){
    
      if(i < 3){
        my_doc %<>% 
          body_add_par(tbls_supp$caption[[i]]) %>% 
          body_add_flextable(tbls_supp$object[[i]]) %>% 
          body_add_break() %>% 
          body_end_section_portrait() 
      }
      
      if(i == 3){
        my_doc %<>% 
          body_add_par(tbls_supp$caption[[i]]) %>% 
          body_add_flextable(tbls_supp$object[[i]]) %>% 
          body_add_break() %>% 
          body_end_section_landscape()
      }
      
      if(i > 3){
        my_doc %<>% 
          body_add_par(tbls_supp$caption[[i]]) %>% 
          body_add_flextable(tbls_supp$object[[i]]) %>% 
          body_add_break() 
      }
    }
    
  }
  
  # Supplemental figures
  
  if(nfig_supp > 0){
    
    for(i in seq(nfig_supp)){
      
      if( inherits(figs_supp$object[[i]], 'gg') ) {
        
        filename <- tempfile(fileext = ".emf")
        
        emf(
          file = filename, 
          width = figs_supp$width[i], 
          height = figs_supp$height[i]
        )
        
        print(figs_supp$object[[i]])
        
        dev.off()
        
      } else {
        
        # This assumes the above has been taken care of already
        filename <- figs_supp$object[[i]]
        
      }
      
      my_doc %<>% 
        body_add_par(figs_supp$caption[i]) %>% 
        body_add_img(
          filename, 
          width = figs_supp$width[i],
          height = figs_supp$height[i]
        )
      
      if( length(figs_supp$legend[i])>0 ){
        
        my_doc %<>% body_add_par(figs_supp$legend[i])
        
      }
      
      my_doc %<>% body_add_break()
      
    }
  }
  
  print(
    my_doc, 
    file.path(
      ".",
      "doc",
      glue("{Sys.Date()}-Number_and_Timing_of_ABPM.docx")
    )
  )
  
}
