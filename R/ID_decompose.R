##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
ID_decompose <- function(data, remove_id = TRUE) {

  out <- data %>% 
    mutate(
      ID = str_replace(
        ID, 
        pattern = 'measurements', 
        replacement = 'measurements xx'
      )
    ) %>% 
    separate(col = ID, 
      into = c('n_msr', 'descr'), 
      sep = ' xx ', 
      remove = remove_id) %>% 
    mutate(
      n_msr = if_else(
        condition = strategy == 'distr',
        str_replace(n_msr, 'BP', 'Distributed BP'),
        str_replace(n_msr, 'BP', 'Consecutive BP')
      )
    )
  
  if(!remove_id) out <- mutate(out, ID = str_replace(ID, ' xx ', ' '))
  
  out

}
