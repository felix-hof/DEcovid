#' Load all possible data sets depending on the resolutions desired
#'
#' @description This function is a wrapper around the various functions that load data sets within this package.
#' It allows the user to define the spatial, temporal and age group resolutions as well as further arguments for all
#' of the functions that download the data sets and receive a list with all of the possible data sets for the desired
#' resolutions.
#'
#' @template spat_res 
#' @template time_res 
#' @template age_res 
#' @param ... Further arguments passed to the functions \code{get_xx} where \code{xx} is a placeholder for any of the
#' functions returning data sets within this package.
#'
#' @return A \code{list} containing all of the data sets that could possibly be used for an Endemic-Epidemic model
#' with the specified resolutions.
#' 
#' @details Internally, this function calls \code{data("overview", package = "DEcovid")} to determine whether a 
#' data set is suitable for the specified resolutions. Of course, downloading all of the data sets for the first
#' time will take some time.
#' 
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' spat_res <- 0
#' time_res <- "weekly"
#' age_res <- "no_age"
#' 
#' data <- get_data(age_res = age_res, spat_res = spat_res, time_res = time_res,
#'                  complete = "region", enforce_cache = FALSE)
#' }
get_data <- function(spat_res, time_res, age_res, ...){
  
  # check args
  opts <- list(spat_res = spat_res, 
               time_res = time_res,
               age_res = age_res)
  choices <- list(spat_res = seq(0L, 3L, 1L),
                  time_res = c("daily", "weekly"),
                  age_res = c("age", "no_age"))
  check <- vapply(seq_along(opts), function(x){
    if(!opts[[x]] %in% choices[[names(opts)[x]]]) FALSE else TRUE
  }, logical(1L))
  
  if(!all(check)){
    msg <- vapply(which(!check), function(x) paste0(names(opts)[x], " must be one of: ", paste0(choices[[x]], collapse = ", ")), character(1L))
    msg <- paste0(msg, collapse = "\n")
    stop(msg)
  }
  
  # get the extra arguments in a list
  utils::data("overview", package = "DEcovid")
  df <- overview
  dotargs <- list(...)
  dotargs <- append(opts, dotargs)
  # what data sets are even considered with current resolutions
  idx_scenario <- vapply(df$include, function(FUN){
    FUN(age_res = age_res, time_res = time_res, spat_res = spat_res)
  }, logical(1L))
  idx_fetch <- df$needs_fetching
  # which of these need to be loaded
  all_covs <- df$dataset[idx_scenario]
  to_fetch <- df$dataset[idx_scenario & idx_fetch]
  # load the datasets that need loading
  data <- lapply(to_fetch, function(y){
    #print(y)
    f <- eval(as.name(paste0("get_", y)), envir = as.environment("package:DEcovid"))
    dotargs_used <- dotargs[names(dotargs) %in% names(formals(f))]
    clist <- append(list(f), dotargs_used)
    eval(as.call(clist))
  })
  names(data) <- to_fetch
  # unlist those that consist of multiple covariates
  is_not_dataframe <- !vapply(data, is.data.frame, logical(1L))
  if(any(is_not_dataframe)){
    nms <- to_fetch[is_not_dataframe]
    nms <- do.call(`c`, lapply(seq_along(nms), 
                               function(x) paste0(nms[x], "_", names(data[is_not_dataframe][[x]]))))
    unlisted <- unlist(data[is_not_dataframe], recursive = FALSE)
    names(unlisted) <- nms
    data <- append(data[!is_not_dataframe], unlisted)
  }
  # add those that do not need any loading
  still_missing <- setdiff(all_covs, to_fetch)
  n_still_missing <- length(still_missing)
  if(n_still_missing != 0L){
    nms <- c(names(data), still_missing)
    data <- append(data, as.list(rep(NA, length(still_missing))))
    names(data) <- nms
  } 
  data
}