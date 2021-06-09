

the_plan <- drake_plan(
  
  # minimal time b/t measures for inclusion
  time_thresh = 1/2, # 30 minutes
  # definition of nocturnal hypertension
  nht_sbp = 120, nht_dbp = 70, 
  # number of bootstrap replicates to use
  boot_iters = 2500,
  
  # control variables for models
  # note: (race is not included b/c all JHS participants are black)
  # (however, race is included automatically for CARDIA models)
  control_vars = c('age','sex','diabetes','currentsmoker',
    'bpmeds','slp_duration'), #'cln_sbp', 'cln_dbp'
  
  # coronary artery risk development in young adults
  cardia_abpm = clean_cardia_abpm(), 
  cardia_y30 = clean_cardia_a1866(),
  
  # jackson heart study
  jhs_abpm = clean_jhs_abpm(), 
  jhs_visit1 = clean_jhs_visit1(),
  
  # keep this separate from abpm until exclusions are done
  visit1_and_y30 = merge_non_abpm_data(cardia_y30, jhs_visit1),
  
  # pooled abpm data
  abpm_long_pooled = bind_rows(
    CARDIA = cardia_abpm,
    JHS = jhs_abpm,
    .id = 'study'
  ) %>%
    group_by(study, subjid), 
  
  # table s1
  exclusions = exclude_participants(abpm_long_pooled, time_thresh),
  
  # apply exclusions to the long data
  abpm_long = filter(abpm_long_pooled, subjid %in% exclusions$data$subjid),
  

  # main analysis data
  abpm_wide = exclusions$data %>%
    group_by(subjid) %>%
    summarize(
      slp_sbp_full = mean(sbp),
      slp_dbp_full = mean(dbp),
      slp_duration = max(tss)
    ) %>%
    left_join(visit1_and_y30, by = 'subjid') %>%
    mutate(
      nht_full = if_else(
        condition = slp_sbp_full >= nht_sbp | slp_dbp_full >= nht_dbp,
        true = 'yes',
        false = 'no'
      )
    ),

  # figure showing estimated mean BP during sleep
  # (requested by reviewer)
  fig_mean_abpm = make_fig_mean_abpm(abpm_long, abpm_wide),
  
  # table 1
  tbl_characteristics = make_tblone(data = abpm_wide),

  # make the bp samplers
  design_init = make_study_design(abpm_long = abpm_long,
    nht_sbp = nht_sbp, nht_dbp = nht_dbp),

  # evaluate their accuracy (and kappa stats)
  design_evaluated = evaluate_bp_samplers(design_init, abpm_wide, boot_iters),
  
  tbl_variations = make_tblvariations(design_evaluated),
  
  tbl_means = make_tblmeans(design_evaluated, abpm_wide),
  
  spearman_cors = make_spcors(design_evaluated),

  # identify top performer in each combo of time_var, n_msr, and strategy
  winning_samplers = design_evaluated %>%
    filter(study == 'Overall') %>% 
    group_by(time_var, strategy, n_msr) %>%
    arrange(desc(kap_nht_est)) %>% 
    slice(1) %>% 
    pull(ID),

  # tabulate kappas and absolute error values
  tbl_accuracy = make_tblacc(winning_samplers, design_evaluated),
  
  n_variations_with_good_kappa = bracket_drop(
    tbl_accuracy$everyone$tbv_kap_Overall) %>% 
    as.numeric() %>% 
    enframe() %>% 
    summarize(answer = sum(value >= 0.80)) %>% 
    pull(answer),

  # bootstrap the kappa differences (huge time sink)
  kappa_comparisons = compare_kappas(design_evaluated,
    winning_samplers, abpm_wide, boot_iters),

  kappa_comparisons_wrt_234 = make_specific_kappa_comparisons(
    design_evaluated,
    winning_samplers,
    abpm_wide,
    boot_iters
  ),
  
  tbl_kappa_comparisons_wrt_234 = make_tblkappa_234(
    kappa_comparisons_wrt_234,
    design_evaluated
  ),
  
  # tile figures of kappa differences
  fig_kappa_comparison = make_figkappa(kappa_comparisons),

  # analyze the study design (fit all models, compute C-stats)
  study_analyzed = analyze_design(
    design_init = design_init,
    abpm_wide = abpm_wide,
    control_vars = control_vars
  ),

  # prevalence ratio and c-statistic table
  tbl_cstats = make_tblcstat(study_analyzed, winning_samplers),

  tbl_prs = make_tblPRs(study_analyzed, winning_samplers),
   
  # fig_errs = make_figerrs(study_analyzed, winning_samplers),
   
  report_tables = make_report_tables(
    tbl_characteristics = tbl_characteristics,
    tbl_means = tbl_means,
    tbl_accuracy = tbl_accuracy,
    tbl_kappa_comparisons_wrt_234 = tbl_kappa_comparisons_wrt_234,
    tbl_exclusions = exclusions$table,
    tbl_variations = tbl_variations,
    tbl_prs = tbl_prs,
    tbl_cstats = tbl_cstats,
    control_vars = control_vars
  )
  
)
