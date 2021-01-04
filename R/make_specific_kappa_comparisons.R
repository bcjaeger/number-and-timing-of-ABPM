##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design
##' @param abpm_wide
##' @param boot_iters
make_specific_kappa_comparisons <- function(design_evaluated, 
                                            winning_samplers,
                                            abpm_wide, 
                                            boot_iters) {

  
  tibble(ID = winning_samplers) %>% 
    mutate(
      reference = if_else(
        str_detect(ID, 'midnight$'),
        '3 BP measurements at 2, 3 and 4 hours after midnight',
        '3 BP measurements at 2, 3 and 4 hours after falling asleep'
      ),
      boot_compare = map2(
        .x = reference, 
        .y = ID, 
        .f = ~compare_specific_kappas(
          design_evaluated = design_evaluated,
          id_1 = .x,
          id_2 = .y,
          abpm_wide = abpm_wide,
          boot_iters = boot_iters
        )
      )
    ) %>% 
    unnest(boot_compare)

  
}
