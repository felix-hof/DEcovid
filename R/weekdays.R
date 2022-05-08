#' Get weekday covariates for a specific time period or the period relevant for this master thesis
#'
#' @template spat_res
#' @template age_res
#' @template start
#' @template end
#' @template reference
#' @template cache_dir
#'
#' @return A list with covariates for each day except the reference day. The covariates are coded
#' as effects rather than contrasts.
#' @details All arguments default to NULL because the function has two use cases. Either the the arguments
#' \code{spat_res} and \code{age_res} have to be set or the arguments \code{start}, \code{end},
#' and \code{reference}.
#'
#' @export
#'
#' @importFrom tidyr expand_grid
#' @importFrom dplyr left_join
#'
#' @examples
#' wkd <- get_weekday(start = as.Date("2020-01-01"), end = as.Date("2020-12-31"), reference = "Monday")
#' wkd <- get_weekday(age_res = "no_age", spat_res = 2, reference = "Monday")
#'
get_weekday <- function(spat_res = NULL,
                        age_res = NULL,
                        start = NULL,
                        end = NULL,
                        reference = NULL,
                        cache_dir = NULL){

  # time_res = NULL; spat_res = NULL; age_res = NULL
  # start = NULL; end = NULL; reference = NULL
  # Check arguments
  if(is.null(reference)) stop("No reference day provided.")
  if(!is.null(start) && !is.null(end)){
    join <- FALSE
  } else if(!is.null(spat_res) && !is.null(age_res)){
    join <- check_res_args(time_res = "daily",
                           spat_res = spat_res,
                           age_res = age_res)
  } else {
    stop("Invalid argument configuration.")
  }

  if(join){
    cache_dir <- get_cache_dir(cache_dir = cache_dir)
    res <- get_case_info(spat_res = spat_res, time_res = "daily", cache_dir = cache_dir)
    vals <- get_weekday_vals(start = res$date[1L], end = rev(res$date)[1L], reference = reference)
    join_object <- tidyr::expand_grid(age = if(age_res == "no_age") "total" else res$age,
                                      date = res$date,
                                      region = res$region)
    out <- lapply(names(vals), function(x){
      dplyr::left_join(join_object, y = vals[[x]], by = "date")
    })
    names(out) <- names(vals)
  } else {
    if(any(vapply(list(start, end, reference), is.null, logical(1L))))
      stop("At least one of the arguments \"start\", \"end\", or \"reference\" is NULL. Please set them correctly.")
    out <- get_weekday_vals(start = start, end = end, reference = reference)
  }
  out
}


#' Get weekday covariates for a specific time frame
#'
#' @template start
#' @template end
#' @template reference
#'
#' @return A list with covariates for each day except the reference day. The covariates are coded
#' as effects rather than contrasts.
#' @noRd
#'
#' @importFrom dplyr tibble case_when
get_weekday_vals <- function(start, end, reference){
  # input checks
  if(length(reference) > 1L || !inherits(reference, "character")) stop("The argument \"reference\" must be a character vector of length 1.")
  if(!all(vapply(list(start,end), class, character(1L)) == "Date")) stop("Arguments \"start\" and \"end\" must have class \"Date\".")
  if(all(c(length(start), length(end)) != c(1L, 1L))) stop("Both the arguments \"start\" and \"end\" must be of length 1.")
  # get weekday names (as they depend on user locale)
  day_names <- weekdays(seq(as.Date("2022-02-07"), as.Date("2022-02-13"), by = 1), abbreviate = FALSE) # monday through sunday
  # check if reference day in day_names
  if(!(reference %in% day_names)) stop("The day passed in \"reference\" must be one of c(", paste0(day_names, collapse = ", "), ").")
  # get weekday sequences
  dates <- seq(start, end, by = 1L)
  wkd <- weekdays(dates)
  out <- lapply(day_names[day_names != reference], function(x){
    dplyr::tibble(date = dates) %>%
      mutate(value = dplyr::case_when(wkd == x ~ 1L,
                                      wkd == reference ~ -1L,
                                      TRUE ~ 0L))
  })
  names(out) <- day_names[day_names != reference]
  return(out)
}


#' Get weekend covariate for a specific time period or the period relevant for this master thesis
#'
#' @template spat_res
#' @template age_res
#' @template start
#' @template end
#' @template cache_dir
#' @return A \code{tibble} with a date and a value column which contain the date sequence from start to end and a binary indicator
#' whether a day is a weekend day.
#' @details All arguments default to NULL because the function has two use cases. Either the the arguments
#' \code{spat_res} and \code{age_res} have to be set or the arguments \code{start} and \code{end}.
#'
#' @export
#' @importFrom tidyr expand_grid
#' @importFrom dplyr left_join
#' @examples
#' wke <- get_weekend(start = as.Date("2020-01-01"), end = as.Date("2020-12-31"))
#' wke <- get_weekend(age_res = "no_age", spat_res = 2)
#'
get_weekend <-function(spat_res = NULL,
                       age_res = NULL,
                       start = NULL,
                       end = NULL,
                       cache_dir = NULL){

  # Check arguments
  if(!is.null(start) && !is.null(end)){
    join <- FALSE
  } else if(!is.null(spat_res) && !is.null(age_res)){
    join <- check_res_args(time_res = "daily",
                           spat_res = spat_res,
                           age_res = age_res)
  } else {
    stop("Invalid argument configuration.")
  }

  if(join){
    cache_dir <- get_cache_dir(cache_dir = cache_dir)
    res <- get_case_info(spat_res = spat_res, time_res = "daily", cache_dir = cache_dir)
    vals <- get_weekend_vals(start = res$date[1L], end = rev(res$date)[1L])
    join_object <- tidyr::expand_grid(age = if(age_res == "no_age") "total" else res$age,
                                      date = res$date,
                                      region = res$region)
    out <- dplyr::left_join(x = join_object, y = vals, by = "date")
  } else {
    if(any(vapply(list(start, end), is.null, logical(1L))))
      stop("At least one of the arguments \"start\" or \"end\"is NULL. Please set them correctly.")
    out <- get_weekend_vals(start = start, end = end)
  }
  out
}


#' Get weekend covariate for a specific time frame
#'
#' @template start
#' @template end
#'
#' @return A \code{tibble} with a date and a value column which contain the date sequence from start to end and a binary indicator
#' whether a day is a weekend day.
#' @noRd
#'
#' @importFrom dplyr tibble
get_weekend_vals <- function(start, end){
  # input checks
  if(!all(vapply(list(start,end), class, character(1L)) == "Date")) stop("Arguments \"start\" and \"end\" must have class \"Date\".")
  if(all(c(length(start), length(end)) != c(1L, 1L))) stop("Both the arguments \"start\" and \"end\" must be of length 1.")
  # get weekday names (as they depend on user locale)
  day_names <- weekdays(seq(as.Date("2022-02-07"), as.Date("2022-02-13"), by = 1), abbreviate = FALSE) # monday through sunday
  # get weekday sequences
  dates <- seq(start, end, by = 1L)
  wkd <- weekdays(dates)
  out <- dplyr::tibble(date = dates,
                       value = ifelse(wkd %in% day_names[6:7], 1L, 0L))
  return(out)
}
