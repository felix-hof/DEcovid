#' Get stringency index from cache or source
#'
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{date} and \code{value}. The column \code{value} contains the
#' stringency index for each day.
#' @export
#'
#' @examples
#' stringency <- get_stringency()
#'
get_stringency <- function(cache_dir = NULL){

  # set parameters for cacheing
  filename <- "stringency.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 1, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_stringency_from_source(cache_dir, filename)
  }

  return(dat)
}

#' Get stringency data from OxCGRT
#'
#' @template cache_dir
#' @param  filename The name of the file where the stringency data is stored.
#'
#' @return A \code{tibble} with columns \code{age}, \code{lvl3} and \code{value} (contains population counts).
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
