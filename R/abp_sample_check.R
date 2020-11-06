##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title check the abpm sample
##' @param data a dataset or tibble
##' @param times the times to obtain BP measurements at
##' @param time_variable character value of column name with time data
##' @param threshold maximum absolute difference between 
##'   target and true measurement time
abp_sample_check <- function(data, times, time_variable, threshold = 1/2) {

  abp_sample <- abp_sample_get(data = data,
    times = times,
    time_variable = time_variable)
  
  if(nrow(abp_sample) < length(times)) return(FALSE)
  
  abs_time_differences <- abs(abp_sample[[time_variable]] - times)
  
  # return TRUE is all of the time differences are < threshold.
  # return FALSE if any time difference is > threshold.
  !any(abs_time_differences > threshold)

}
