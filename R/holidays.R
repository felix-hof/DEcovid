#' Get holiday data from appfield.net or from cache
#'
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{date}, \code{lvl3} and \code{value} (contains the binary holiday indicator).
#' @export
#'
#' @examples
#' holidays <- get_holidays()
#'
get_holidays <- function(cache_dir = NULL){

  # set parameters for cacheing
  filename <- "holidays.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 10, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_holidays_from_source(cache_dir, filename)
  }

  return(dat)
}

#' Get holiday data from appfield.net
#'
#' @template cache_dir
#' @param  filename The name of the file that the holiday data is stored in.
#'
#' @return A \code{tibble} with columns \code{date}, \code{lvl3} and \code{value} (contains the binary holiday indicator).
#'
#' @importFrom rvest read_html html_elements html_text html_children
#' @importFrom magrittr extract
#' @importFrom dplyr tibble left_join select distinct mutate right_join rename arrange if_else
#' @importFrom stringr str_extract_all
#'
get_holidays_from_source <- function(cache_dir, filename){

  # other stuff needed
  ref <- nuts_table()
  all_dates <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = 1)

  # get holiday data
  # get holiday dates for 2020 & 2021
  hols2020 <- "https://urlaubstage-planen.de/feiertage-in-deutschland-2020.htm" %>%
    rvest::read_html()
  hols2021 <- "https://urlaubstage-planen.de/feiertage-in-deutschland-2021.htm" %>%
    rvest::read_html()

  # get national holidays for all of Germany 2020
  ph_de2020 <- hols2020 %>%
    rvest::html_elements(".with-values") %>%
    rvest::html_text() %>%
    strsplit(" - ") %>%
    lapply(function(x){
      x[1]
    }) %>%
    unlist()

  # get national holidays for all of Germany 2021
  ph_de2021 <- hols2021 %>%
    rvest::html_elements(".with-values") %>%
    rvest::html_text() %>%
    strsplit(" - ") %>%
    lapply(function(x){
      x[1]
    }) %>%
    unlist()

  # get Bundesland specific holidays 2020
  bl2020 <- hols2020 %>%
    rvest::html_elements(".content-frame-filled") %>%
    magrittr::extract(3) %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    magrittr::extract(2:length(.)) %>%
    magrittr::extract(!(seq_along(.) %in% seq(2, length(.), 3)))

  # get Bundesland specific holidays 2021
  bl2021 <- hols2021 %>%
    rvest::html_elements(".content-frame-filled") %>%
    magrittr::extract(3) %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    magrittr::extract(2:length(.)) %>%
    magrittr::extract(!(seq_along(.) %in% seq(2, length(.), 3)))

  bl <- ref %>% dplyr::arrange(lvl1) %>%
    dplyr::select(lvl1_name) %>%
    dplyr::distinct() %>%
    dplyr::rename(name = lvl1_name)

  # use Eurostat abbreviations
  bl_codes <- bl %>%
    dplyr::left_join(ref %>% dplyr::select(lvl1, lvl1_name) %>% dplyr::distinct(), by = c("name" = "lvl1_name")) %>%
    dplyr::select(lvl1) %>% unlist(use.names = FALSE)

  # loop over all Bundeslaender
  res <- lapply(seq_along(bl), function(x){
    # get holiday vector
    dates <- c(
      # national holidays 2020
      ph_de2020,
      # BL specific holidays 2020
      stringr::str_extract_all(bl2020[grep(bl[x], bl2020) + 1], "\\d{2}\\.\\d{2}\\.\\d{4}") %>% unlist(),
      # national holidays 2021
      ph_de2021,
      # BL specific holidays 2020
      stringr::str_extract_all(bl2021[grep(bl[x], bl2021) + 1], "\\d{2}\\.\\d{2}\\.\\d{4}") %>% unlist()
    ) %>%
      as.Date(format = "%d.%m.%Y")

    # expand to entire time series
    out <- dplyr::tibble(date = all_dates) %>%
      dplyr::mutate(holiday = dplyr::if_else(date %in% dates, 1L, 0L),
                    lvl1 = bl_codes[x])

    return(out)
  }) %>%
    dplyr::bind_rows()

  # Expand this to lvl3 regions
  dat <- tidyr::expand_grid(lvl3 = ref$lvl3,
                     date = all_dates) %>%
    dplyr::mutate(lvl1 = substr(lvl3, 1, 3)) %>%
    dplyr::left_join(res, by = c("date" = "date", "lvl1" = "lvl1")) %>%
    dplyr::select(date, lvl3, holiday) %>%
    dplyr::rename(value = holiday) %>%
    dplyr::arrange(lvl3, date)

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
