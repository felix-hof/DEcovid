#' Get the proportion of unvaccinated people from impfdashboard.de or from cache
#'
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{date} and \code{value} (contains log-proportion of unvaccinated people).
#' @export
#'
#' @examples
#' vaccination <- get_vaccination()
#'
get_vaccination <- function(cache_dir = NULL){

  # set parameters for cacheing
  filename <- "vaccination.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 1, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_vaccination_from_source(cache_dir, filename)
  }

  return(dat)
}

#' Get the proportion of unvaccinated people from impfdashboard.de
#'
#' @template cache_dir
#' @param  filename The name of the file where the vaccination data is stored.
#'
#' @return A \code{tibble} with columns \code{date} and \code{value} (contains log-proportion of unvaccinated people).
#'
#' @importFrom readr read_tsv
#' @importFrom dplyr select mutate rename left_join arrange
#'
get_vaccination_from_source <- function(cache_dir, filename){

  # get population data to calculate rate
  total_population <- sum(get_population(cache_dir)$value)

  # get data
  dat <- "https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv" %>%
    readr::read_tsv(url, show_col_types = FALSE, progress = FALSE, trim_ws = TRUE) %>%
    {
      if(all(grepl("^X", colnames(.)))) colnames(.) <- .[1, ] %>% unlist(use.names = FALSE)
      .[2:nrow(.), ]
    } %>%
    dplyr::select(date, personen_erst_kumulativ) %>%
    # get proportion of people who received at least one dose (cap at 1)
    dplyr::mutate(personen_erst_kumulativ = as.numeric(personen_erst_kumulativ) / total_population,
                  personen_erst_kumulativ = ifelse(personen_erst_kumulativ > 1,
                                                   1,
                                                   personen_erst_kumulativ),
                  date = as.Date(date)) %>%
    # get proportion of unvaccinated people by inversion
    dplyr::mutate(personen_erst_kumulativ = 1 - personen_erst_kumulativ) %>%
    dplyr::rename(unvac = personen_erst_kumulativ) %>%
    # extend time series such that it starts from 2020-01-01
    dplyr::left_join(dplyr::tibble(date = seq(as.Date("2020-01-01"), max(.$date), by = 1)),
                     .,
                     by = "date") %>%
    # insert 1 before the start of this data set
    {
      idx <- which(is.na(.$unvac))
      if(idx[1] == 1 & all(diff(idx) == 1)){
        .$unvac[idx] <- 1
      } else {
        .$unvac[1:which(diff(idx) != 1)] <- 1
      }
      .
    } %>%
    dplyr::mutate(unvac = log(unvac)) %>%
    dplyr::rename(log_unvac = unvac) %>%
    dplyr::arrange(date) %>%
    dplyr::rename(value = log_unvac)

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
