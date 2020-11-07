##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author bcjaeger
##' @export
table_polisher <- function(ft,
                           font_size = 11,
                           font_name = "Calibri"){
  
  ft %>%
    font(fontname = font_name, part = 'all') %>%
    fontsize(size = font_size, part = 'all') %>%
    height(height = 2, part = 'footer')
  
}
