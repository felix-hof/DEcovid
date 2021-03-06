#' Get stringency index for Germany from \insertCite{DEcovid:Stringency;textual}{DEcovid}
#'
#' @description The data set is retrieved from \insertCite{DEcovid:Stringency;textual}{DEcovid} and contains the stringency index. This
#' is an index that quantifies the strictness of government imposed polcies within the context of the COVID-19 pandemic. The index is 
#' composed from various sub-indices that each relate to different types of policies. Some of the sub-indices directly describe anti-contagion
#' policies but various other measures such as economical support for businesses are also reflected. For more information on the stringency index
#' see the link in the references section.
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{tibble} with columns \code{date} and \code{value}. The column \code{value} contains the
#' stringency index for each day. If the arguments \code{time_res}, \code{spat_res}, and \code{age_res} are specified,
#' the index is expanded over the regions and age groups defined by the case data set that can be accesed via \code{\link[DEcovid]{get_cases}}.
#' If weekly data is requested, the average stringency index for each specific calendar week is returned.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @importFrom tidyr expand_grid
#'
#' @export
#' 
#' @references 
#' \insertRef{DEcovid:Stringency}{DEcovid}
#'
#' @examples
#' stringency <- get_stringency()
#'
get_stringency <- function(time_res = NULL,
                           spat_res = NULL,
                           age_res = NULL,
                           cache_dir = NULL,
                           enforce_cache = FALSE){

  # Check inputs
  join <- check_res_args(time_res = time_res,
                         spat_res = spat_res,
                         age_res = age_res)
  check_enforce_cache(enforce_cache = enforce_cache)

  # set parameters for cacheing
  filename <- "stringency.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 1, units = "days")

  # get pre-processed data from file or from source
  if(enforce_cache){
    if(!file.exists(make_path(cache_dir, filename))){
      stop("There is no cached version of the requested data in 'cache_dir' directory.")
    } else {
      dat <- readRDS(make_path(cache_dir, filename))
    }
  } else {
    if(from_cache){
      dat <- readRDS(make_path(cache_dir, filename))
    } else {
      dat <- get_stringency_from_source(cache_dir, filename)
    }
  }

  # aggregate if desired
  if(join){
    dims <- get_case_info(spat_res = 3, time_res = "daily", cache_dir = cache_dir)
    region <- dims$region
    age <- dims$age
    date <- dims$date
    dat <- tidyr::expand_grid(age = age, date = date, region = region) %>%
      dplyr::left_join(y = dat, by = "date") %>%
      summarise_data(time_res = time_res, spat_res = spat_res, age_res = age_res,
                     time_f = time_f_stringency, spat_f = spat_f_stringency, age_f = age_f_stringency)
  }

  return(dat)
}

#' Get stringency data from OxCGRT
#'
#' @template cache_dir
#' @param  filename The name of the file where the stringency data is stored.
#'
#' @return A \code{tibble} with columns \code{date}  and \code{value} (contains stringency index).
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter select mutate rename arrange
#'
get_stringency_from_source <- function(cache_dir, filename){

  # Get data
  dat <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv" %>%
    readr::read_csv(trim_ws = TRUE, progress = FALSE,
                    show_col_types = FALSE) %>%
    dplyr::filter(CountryCode == "DEU") %>%
    dplyr::select(Date, StringencyIndex) %>%
    dplyr::mutate(Date = as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\30", Date))) %>%
    dplyr::rename(date = Date, value = StringencyIndex) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(!is.na(value))

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
