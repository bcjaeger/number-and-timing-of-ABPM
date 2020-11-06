##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param style 
##' @param n 
make_footers <- function(style = c('symbol', 'letter', 'number'), n = 10){
  
  if(n > 10) stop("too many footnotes.", call.=FALSE)
  
  foot_symbols <- c('*', '†', '‡', '§', '||', '¶', '#', '**', '††', '‡‡')[1:n]
  foot_letters <- letters[1:n]
  foot_numbers <- as.character(1:n)
  
  output <- switch (style[1],
                    'symbol' = foot_symbols,
                    'letter' = foot_letters,
                    'number' = foot_numbers
  )
  
  output <- paste0(output, ' ')
  
  output
  
}
