#' Get the available time period for fitting of Endemic-Epidemic (EE) models
#'
#' @description This function returns the start and the end date between which there is data available for all data sets within a list. This helps to truncate time
#' the data sets that can be accessed via the package \code{DEcovid} but also for other applications. The idea is that for model fitting, \code{NA} values are usually
#' not desired in such time series. Therefore, the time series must usually be truncated to a period where there is non-missing data available for all predictor variables
#' as well as the response variable. This function thus returns the longest period without missing values in any of the data sets within the list passed in the first 
#' argument.
#'
#' @param data_list A \code{list} containing possible covariate \code{data.frame}s. 
#' These \code{data.frame}s must have a column named \code{date} which contains objects of class \code{Date} and a column named \code{value} which can contain any type of 
#' data. This is usually the output of the function \code{\link[DEcovid]{get_data}}.
#' @param time_res The temporal resolution that the data frames have. Must be either \code{"daily"} or \code{"weekly"}.
#'
#' @return The function returns a vector of length 2 containing the start and end date of the the longest run where all data
#' values in the data frames within \code{data_list} are not \code{NA}.
#' @export
#' 
#' @importFrom stats lag
#'
#' @examples
#' \dontrun{
#' spat_res <- 0
#' time_res <- "weekly"
#' age_res <- "no_age"
#' 
#' data <- get_data(age_res = age_res, spat_res = spat_res, time_res = time_res,
#'                  complete = "region", enforce_cache = FALSE)
#' dates <- get_time_period(data_list = data, time_res = time_res)
#' }
#' 
get_time_period <- function(data_list, time_res){
  
  if(length(time_res) != 1L) stop("Argument time_res must be of length 1.")
  if(!time_res %in% c("daily", "weekly")) stop("Argument time_res must be one of c(\"daily\", \"weekly\").")
  if(!is.list(data_list)) stop("Argument data_list must be a list.")
  if(!all(vapply(data_list, function(x) is.list(x) || is.na(x), logical(1L)))) stop("Elements of data_list must either be NA or a data frame.")
  idx <- vapply(data_list, is.data.frame, logical(1L))
  has_date <- vapply(data_list[idx], function(x) "date" %in% colnames(x) && inherits(x$date, "Date"), logical(1L))
  if(!all(has_date)) stop("All data frames in data_list must have a column \"date\" which contains objects of class \"Date\".")
  
  # get index of those that are actual data frames (seasonality is not a data frame)
  idx <- vapply(data_list, is.data.frame, logical(1L))
  # get the number of units
  n_units <- unique(vapply(data_list[idx], function(y) nrow(unique(y[, c("age", "region")])), integer(1L)))
  if(length(n_units) != 1L) stop("There should be the same number of units in all covariates.")
  # how many times would we expect a date to be in there
  expected_times <- sum(idx) * n_units
  # get all available dates in all data frames
  avail_dates <- sort(do.call(`c`, lapply(data_list[idx], function(x) x$date[!is.na(x$value)])))
  # get unique dates
  unique_dates <- unique(avail_dates)
  # count how many times each of the dates appears in all data frames
  non_na_counts <- vapply(unique_dates, function(x) sum(avail_dates == x), integer(1L))
  # get those dates that do not have an NA value in any of the data frames
  non_na_dates <- unique_dates[non_na_counts == expected_times]
  # set expected difference between each of the dates
  expected_diff <- if(time_res == "daily") 1L else if(time_res == "weekly") 7L else stop("Invalid time resolution.")
  # find longest run where the diff is regular
  dff <- as.integer(diff(non_na_dates))
  dff <- ifelse(dff == expected_diff, 1L, 0L)
  rl <- rle(dff)
  end <- cumsum(rl$lengths)
  start <- c(1L, stats::lag(end)[-1L] + 1L)
  idx <- which(end - start + 1L == rl$lengths[rl$values == 1L])
  out <- non_na_dates[c(start[idx], end[idx] + 1L)]
  names(out) <- c("start", "end")
  out
}