#' Get the case data from RKI repository
#'
#' @param time_res A \code{character} vector of length 1 indicating the temporal resolution of the data.
#' Accepted values are either \code{"daily"} or \code{"weekly"}.
#' @param spat_res A \code{numeric} vector of length 1 indicating the spatial resolution of the data.
#' Accepted values are \code{0}, \code{1}, \code{2} and \code{3}. Corresponds to the respective NUTS level.
#' @param age_res A \code{character} vector of length 1 indicating whether or not the data should be stratified by
#' age groups. Accepted values are \code{"age"} and \code{"no_age"}.
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{age}, \code{date}, \code{value} (contains the counts) and \code{region}.
#' The variables are aggregated to the desired level.
#' @export
#'
#' @examples
#' \dontrun{
#' cases <- get_cases(time_res = "daily", spat_res = 3, age_res = "age")
#' cases <- get_cases(time_res = "weekly", spat_res = 3, age_res = "age")
#' cases <- get_cases(time_res = "daily", spat_res = 1, age_res = "age")
#' cases <- get_cases(time_res = "daily", spat_res = 3, age_res = "no_age")
#' }
#'
#'
get_cases <- function(time_res = "daily",
                      spat_res = 3,
                      age_res = "age",
                      cache_dir = NULL){

  # check inputs
  if(!all(c(length(time_res), length(spat_res), length(age_res)) == c(1L, 1L, 1L))){
    stop("The arguments 'time_res', 'spat_res' and 'age_res' must all be of length 1.")
  }
  time_res <- match.arg(time_res, choices = c("daily", "weekly"), several.ok = FALSE)
  spat_res <- match.arg(as.character(spat_res), choices = as.character(0:3), several.ok = FALSE) %>% as.integer()
  age_res <- match.arg(age_res, choices = c("age", "no_age"), several.ok = FALSE)

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

  # save agegroups (used for neighbourhood matrices)
  dat %>%
    pull(age) %>%
    save_agegroups(age = ., path = make_path(cache_dir, "agegroups.rds"))


  # summarise according to specifications
  if(time_res != "daily" || spat_res != 3 || age_res != "age"){
    dat <- summarise_data(data = dat,
                          time_res = time_res,
                          time_f = time_f_cases,
                          spat_res = spat_res,
                          spat_f = spat_f_cases,
                          age_res = age_res,
                          age_f = age_f_cases)
  }

  return(dat)
}


save_agegroups <- function(age, path){
  start_age <- suppressWarnings(as.numeric(gsub("^(\\d+).*$", "\\1", unique(age))))
  ages <- start_age[order(start_age, na.last = NA)]
  saveRDS(ages, file = path)
  return(invisible(0))
}

#' Get case data from the source
#'
#' @template cache_dir
#' @param  filename_cases The name of the file where cases are stored.
#' @param filename_deaths The name of the file where deaths are stored.
#'
#' @return A \code{tibble} with columns \code{age}, \code{date}, \code{value} (contains the counts) and \code{lvl3}.
#'
#' @importFrom readr read_csv cols_only
#' @importFrom magrittr %>% set_attr
#' @importFrom dplyr mutate group_by summarise right_join left_join select rename arrange if_else
#' @importFrom tidyr expand_grid
#'
get_cases_from_source <- function(cache_dir, filename_cases, filename_deaths){

  # get nuts table
  lk_info <- nuts_table(cache_dir) %>%
    dplyr::select(adm_unit, lvl3) %>%
    dplyr::mutate(adm_unit = as.integer(adm_unit))

  ### Change order a bit <- join NUTS correspondence early on and then use these to summarise


  # Get cases data set from RKI via ESRI
  rki_data <- "https://opendata.arcgis.com/api/v3/datasets/e408ccf8878541a7ab6f6077a42fd811_0/downloads/data?format=csv&spatialRefId=4326" %>%
    #"~/Downloads/RKI_COVID-19.csv" %>%
    readr::read_csv(col_names = TRUE,
                    col_types = readr::cols_only(
                      IdLandkreis = "i",
                      Altersgruppe = "c",
                      Meldedatum = "c",
                      NeuerFall = "i",
                      NeuerTodesfall = "i",
                      AnzahlFall = "i",
                      AnzahlTodesfall = "i",
                      Datenstand = "c"),
                    trim_ws = TRUE,
                    progress = FALSE,
                    show_col_types = FALSE) %>%
    magrittr::set_attr(x = .,
                       which = "Datenstand",
                       value = as.Date(sub(",.+", "", .$Datenstand[1]), format = "%d.%m.%Y")) %>%
    dplyr::mutate(Meldedatum = as.Date(gsub("\\s.+$", "", Meldedatum), format = "%Y/%m/%d"),
                  Altersgruppe = gsub("A", "", Altersgruppe),
                  Altersgruppe = ifelse(Altersgruppe == "unbekannt", "unknown", Altersgruppe)) %>%
    # join NUTS IDs and get rid of the Landkreis ID
    dplyr::left_join(lk_info, by = c("IdLandkreis" = "adm_unit")) %>%
    dplyr::select(-IdLandkreis) %>%
    dplyr::group_by(Altersgruppe, Meldedatum, lvl3) %>%
    dplyr::summarise(new_cases = sum(AnzahlFall[NeuerFall >= 0]),
                     new_deaths = sum(AnzahlTodesfall[NeuerTodesfall >= 0]),
                     .groups = "drop") %>%
    # construct complete time series
    {
      date <- seq(min(.$Meldedatum), max(.$Meldedatum), by = 1)
      lkid <- unique(.$lvl3)
      lkid <- lkid[order(lkid)]
      agegrp <- unique(.$Altersgruppe)
      agegrp <- agegrp[order(agegrp)]
      dplyr::right_join(., y = tidyr::expand_grid("Meldedatum" = date,
                                                  "lvl3" = lkid,
                                                  "Altersgruppe" = agegrp),
                        by = c("lvl3" = "lvl3",
                               "Meldedatum" = "Meldedatum",
                               "Altersgruppe" = "Altersgruppe"))
    } %>%
    # fill NAs
    dplyr::mutate(new_cases = dplyr::if_else(is.na(new_cases), 0L, new_cases),
                  new_deaths = dplyr::if_else(is.na(new_deaths), 0L, new_deaths)) %>%
    # prettify
    dplyr::rename(date = Meldedatum,
                  age = Altersgruppe,
                  region = lvl3) %>%
    dplyr::arrange(region, date, age)

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
