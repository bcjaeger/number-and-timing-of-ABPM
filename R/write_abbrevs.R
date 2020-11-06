##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x 
##' @param sep 
##' @param last 
##' @param alphabetical_order 
write_abbrevs <- function(
  x, 
  sep = '; ', 
  last = '; ', 
  alphabetical_order = TRUE
){
  
  if(alphabetical_order){
    x_names_alpha <- sort(names(x))
    x <- x[x_names_alpha]
  }
  
  glue("{names(x)} = {x}") %>% 
    glue_collapse(sep = sep, last = last)
  
}

