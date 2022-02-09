#' Get the case data from RKI repository
#'
#' @template time_res
#' @template spat_res
#' @template age_res
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
  check_res_args(time_res = time_res,
                 spat_res = spat_res,
                 age_res = age_res)

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

  # save agegroups, regions and dates (used for neighbourhood matrices and truncating the timeframe)
  dat %>%
    dplyr::pull(age) %>%
    save_agegroups(age = ., path = make_path(cache_dir, "agegroups.rds"))
  dat %>%
    dplyr::pull(region) %>%
    save_regions(regions = ., prefix = "case_regions_", cache_dir = cache_dir)
  dat %>%
    dplyr::pull(date) %>%
    save_dates(dates = .,
               file_daily = "case_dates_daily.rds",
               file_weekly = "case_dates_weekly.rds",
               cache_dir = cache_dir)


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


# Function to save all age groups that start with a sequence of digits (needed for nb matrices)
save_agegroups <- function(age, path){
  age <- unique(age)
  start_age <- suppressWarnings(as.numeric(gsub("^(\\d+).*$", "\\1", age)))
  ages <- age[order(start_age, na.last = NA)]
  saveRDS(ages, file = path)
  return(invisible(0))
}

# Function to save all NUTS regions of all levels within the data set (needed for nb matrices)
save_regions <- function(regions, prefix, cache_dir){
  regions <- unique(regions)
  vapply(0:3, function(x){ # loop over NUTS levels
    out <- unique(substr(regions, start = 1, stop = x + 2))
    saveRDS(out, file = make_path(cache_dir, paste0(prefix, x,".rds")))
    return(0L)
  }, integer(1L))
  return(invisible(0))
}

# Function to save all dates within the data set (needed to truncate the data sets)
#' @importFrom ISOweek date2ISOweek ISOweek2date
save_dates <- function(dates, file_daily, file_weekly, cache_dir){
  dates <- sort(unique(dates), decreasing = FALSE)
  dates_w <- ISOweek::ISOweek2date(unique(gsub("\\d$", "4", ISOweek::date2ISOweek(dates))))
  saveRDS(dates, file = make_path(cache_dir, file_daily))
  saveRDS(dates_w, file = make_path(cache_dir, file_weekly))
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
