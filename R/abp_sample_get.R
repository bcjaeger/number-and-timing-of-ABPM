

##' @title sample abpm times
##' @param data a dataset or tibble
##' @param times the times to obtain BP measurements at
##' @param time_variable character value of column name with time data

abp_sample_get <- function(data, times, time_variable) {

  time_indx <- vector(mode='integer', length = length(times))
  time_vals <- data[[time_variable]]
  
  # if the data have less rows than the number of measurements required,
  # there aren't enough measurements. 
  if(nrow(data) < length(times)){
    return(data)
  }
  
  for(i in seq_along(time_indx)){
    
    # find closest measurement time to target time
    min_indx <- which.min(abs(time_vals - times[i]))
    
    # the measurement time that is closest to target time
    min_value <- time_vals[min_indx]
    
    # index in data where time = the closest to target time
    new_indx <- which(data[[time_variable]]==min_value)
    
    if(length(new_indx)==0) 
      stop("could not find minimum time", call. = FALSE)
    
    # save the index for later
    time_indx[i] <- new_indx
    
    # remove the time value from the list of candidate time values
    time_vals <- time_vals[-min_indx]
    
  }
  
  data[time_indx, , drop = FALSE]

}
