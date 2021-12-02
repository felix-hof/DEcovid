#' Get the case data from RKI repository
#'
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{age}, \code{date}, \code{value} (contains the counts) and \code{lvl3}.
#' @export
#'
#' @examples
#'cases <- get_cases()
#'
get_cases <- function(cache_dir = NULL){

  # set parameters for cacheing
  filename_cases <- "cases.rds"
  filename_deaths <- "deaths.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename_cases,
                                cutoff = 1, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename_cases))
  } else {
    dat <- get_cases_from_source(cache_dir, filename_cases = filename_cases, filename_deaths = filename_deaths)
  }

  return(dat)
}

#' Get case data from the source
#'
#' @template cache_dir
#' @param  filename_cases The name of the file where cases are stored.
#' @param filename_deaths The name of the file where deaths are stored.
#'
#' @return A \code{tibble} with columns \code{age}, \code{date}, \code{value} (contains the counts) and \code{lvl3}.
#'
#' @importFrom readr read_csv
#' @importFrom magrittr %>% set_attr
#' @importFrom dplyr mutate group_by summarise filter right_join left_join select rename arrange
#' @importFrom tidyr expand_grid
#'
get_cases_from_source <- function(cache_dir, filename_cases, filename_deaths){

  # get nuts table
  lk_info <- nuts_table(cache_dir)

  # Get cases data set from RKI via ESRI
  rki_data <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data" %>%
    readr::read_csv(col_names = TRUE,
                    trim_ws = TRUE,
                    col_types = "----c-iicccnn-----",
                    progress = FALSE,
                    show_col_types = FALSE) %>%
    magrittr::set_attr(x = .,
                       which = "Datenstand",
                       value = as.Date(sub(",.+", "", .$Datenstand[1]), format = "%d.%m.%Y")) %>%
    dplyr::mutate(Meldedatum = as.Date(gsub("\\s.+$", "", Meldedatum), format = "%Y/%m/%d"),
                  Altersgruppe = gsub("A", "", Altersgruppe)) %>%
    dplyr::group_by(Altersgruppe, Meldedatum, IdLandkreis) %>%
    dplyr::summarise(new_cases = sum(AnzahlFall[NeuerFall >= 0]),
                     new_deaths = sum(AnzahlTodesfall[NeuerTodesfall >= 0]),
                     .groups = "drop") %>%
    # # filter unknown age groups
    # dplyr::filter(Altersgruppe != "unbekannt") %>%
    # construct complete time series
    {
      date <- seq(min(.$Meldedatum), max(.$Meldedatum), by = 1)
      lkid <- unique(.$IdLandkreis)
      lkid <- lkid[order(lkid)]
      agegrp <- unique(.$Altersgruppe)
      agegrp <- agegrp[order(agegrp)]
      dplyr::right_join(., y = tidyr::expand_grid("Meldedatum" = date,
                                                  "IdLandkreis" = lkid,
                                                  "Altersgruppe" = agegrp),
                        by = c("IdLandkreis" = "IdLandkreis",
                               "Meldedatum" = "Meldedatum",
                               "Altersgruppe" = "Altersgruppe"))
    } %>%
    # fill NAs
    dplyr::mutate(new_cases = dplyr::if_else(is.na(new_cases), 0L, new_cases),
                  new_deaths = dplyr::if_else(is.na(new_deaths), 0L, new_deaths)) %>%
    # join NUTS IDs
    dplyr::left_join(y = lk_info %>% dplyr::select(adm_unit, lvl3),
                     by = c("IdLandkreis" = "adm_unit")) %>%
    # prettify
    dplyr::select(-IdLandkreis) %>%
    dplyr::rename(date = Meldedatum, age = Altersgruppe) %>%
    dplyr::arrange(lvl3, date, age)

  # save this
  cases <- rki_data %>%
    dplyr::select(-new_deaths) %>%
    dplyr::rename(value = new_cases)

  saveRDS(cases, file = file.path(cache_dir, filename_cases))

  rki_data %>%
    dplyr::select(-new_cases) %>%
    dplyr::rename(value = new_deaths) %>%
    saveRDS(., file = file.path(cache_dir, filename_deaths))

  return(cases)
}
