#' Get the case data from \insertCite{DEcovid:RKIcases;textual}{DEcovid}.
#' 
#' @description This function returns the COVID-19 case counts in Germany which are published by \insertCite{DEcovid:RKIcases;textual}{DEcovid}. The data is stratified by 
#' Landkreis (NUTS-3 level), age groups (0-4, 5-14, 15-34, 35-59, 60-79, 80+) and is available on a daily temporal resolution. The function can also aggregate the data 
#' to other resolutions which is done by summing the case counts. For more information on the data set see the links in the Reference section.
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#' @template enforce_cache
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
#' @references 
#' \insertRef{DEcovid:RKIcases}{DEcovid}
#'
get_cases <- function(time_res = "daily",
                      spat_res = 3,
                      age_res = "age",
                      cache_dir = NULL,
                      enforce_cache = FALSE){

  # check inputs
  check_res_args(time_res = time_res,
                 spat_res = spat_res,
                 age_res = age_res)
  check_enforce_cache(enforce_cache)

  # set parameters for cacheing
  filename_cases <- "cases.rds"
  filename_deaths <- "deaths.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename_cases,
                                cutoff = 1, units = "days")

  # get pre-processed data from file or from source
  if(enforce_cache){
    if(!file.exists(make_path(cache_dir, filename_cases))){
      stop("There is no cached version of the requested data in 'cache_dir' directory.")
    } else {
      dat <- readRDS(make_path(cache_dir, filename_cases))
    }
  } else {
    if(from_cache){
      dat <- readRDS(make_path(cache_dir, filename_cases))
    } else {
      dat <- get_cases_from_source(cache_dir, filename_cases = filename_cases, filename_deaths = filename_deaths)
    }
  }

  # save agegroups, regions and dates (used for neighbourhood matrices and truncating the timeframe)
  if(!any(enforce_cache, from_cache)){
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
  }

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
  cat(paste0("Created age groups used in case data set in '", path, "'."), fill = TRUE)
  return(invisible(0))
}

# Function to save all NUTS regions of all levels within the data set (needed for nb matrices)
save_regions <- function(regions, prefix, cache_dir){
  regions <- unique(regions)
  vapply(0:3, function(x){ # loop over NUTS levels
    out <- unique(substr(regions, start = 1, stop = x + 2))
    saveRDS(out, file = make_path(cache_dir, paste0(prefix, x,".rds")))
    cat(paste0("Created NUTS-", x," levels used in case data set in '", make_path(cache_dir, paste0(prefix, x,".rds")), "'."), fill = TRUE)
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
  cat(paste0("Created daily dates used in the case data set in '", make_path(cache_dir, file_daily), "'."), fill = TRUE)
  saveRDS(dates_w, file = make_path(cache_dir, file_weekly))
  cat(paste0("Created weekly dates used in the case data set in '", make_path(cache_dir, file_weekly), "'."), fill = TRUE)
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
#' @importFrom dplyr mutate group_by summarise right_join left_join select rename arrange if_else bind_rows
#' @importFrom tidyr expand_grid
#' @importFrom parallel mclapply
#' @noRd
get_cases_from_source <- function(cache_dir, filename_cases, filename_deaths){

  # get nuts table
  lk_info <- nuts_table(cache_dir) %>%
    dplyr::select(adm_unit, lvl3) %>%
    dplyr::mutate(adm_unit = as.integer(adm_unit))
  
  # avoid error in R CMD CHECK
  # code taken from https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    num_workers <- 2L
  } else {
    # use all cores in devtools::test()
    num_workers <- parallel::detectCores() - 1
  }

  # Get cases data set from RKI via ESRI
  rki_data <- list(hamburg = "https://opendata.arcgis.com/api/v3/datasets/ab2c1b9c36734faf937cd83dee339517_0/downloads/data?format=csv&spatialRefId=4326",
                   sachsen = "https://opendata.arcgis.com/api/v3/datasets/3d3235c08d4f44a2afd088546b704902_0/downloads/data?format=csv&spatialRefId=4326",
                   nsachsen = "https://opendata.arcgis.com/api/v3/datasets/14d82a9addf841789cd6ef5c1f67476a_0/downloads/data?format=csv&spatialRefId=4326",
                   hessen = "https://opendata.arcgis.com/api/v3/datasets/3ed997d4a8a447f09ab122a1a432b070_0/downloads/data?format=csv&spatialRefId=4326",
                   mv = "https://opendata.arcgis.com/api/v3/datasets/d6c27576ee034bb78621012738615598_0/downloads/data?format=csv&spatialRefId=4326",
                   thuringen = "https://opendata.arcgis.com/api/v3/datasets/790f5423e03e49c4baec55a1a232c136_0/downloads/data?format=csv&spatialRefId=4326",
                   saarland = "https://opendata.arcgis.com/api/v3/datasets/0e59e1262dba4f5f8d6a904113bf7c99_0/downloads/data?format=csv&spatialRefId=4326",
                   rheinlandPfalz = "https://opendata.arcgis.com/api/v3/datasets/57e385f51a07495cb0a1e00a55ee1b5b_0/downloads/data?format=csv&spatialRefId=4326",
                   sachsenAnhalt = "https://opendata.arcgis.com/api/v3/datasets/06a1c943a9b845968b5ad0607f5f48f5_0/downloads/data?format=csv&spatialRefId=4326",
                   schleswig = "https://opendata.arcgis.com/api/v3/datasets/4a648483aedd49b8a6655290181d4c2a_0/downloads/data?format=csv&spatialRefId=4326",
                   berlin = "https://opendata.arcgis.com/api/v3/datasets/3949d6fd2dc74386b763e451f4c6e384_0/downloads/data?format=csv&spatialRefId=4326",
                   bayern = "https://opendata.arcgis.com/api/v3/datasets/45258e51f57d43efb612f700a876ae8f_0/downloads/data?format=csv&spatialRefId=4326",
                   brandenburg = "https://opendata.arcgis.com/api/v3/datasets/5f81692e203a4888a64cb1976aafbd34_0/downloads/data?format=csv&spatialRefId=4326",
                   badenWurttemberg = "https://opendata.arcgis.com/api/v3/datasets/8a0b7d7c9fb442ffaa512221cf11366e_0/downloads/data?format=csv&spatialRefId=4326",
                   nrw = "https://opendata.arcgis.com/api/v3/datasets/a99afefd4258435f8af660b6cbed9bf7_0/downloads/data?format=csv&spatialRefId=4326",
                   bremen = "https://opendata.arcgis.com/api/v3/datasets/f7bdcbe7188545daabe65e6c9e2a4379_0/downloads/data?format=csv&spatialRefId=4326") %>%
    parallel::mclapply(function(x){
      readr::read_csv(x,
                      col_names = TRUE,
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
                      show_col_types = FALSE)
    }, mc.preschedule = FALSE, mc.set.seed = FALSE, mc.cores = num_workers) %>%
    dplyr::bind_rows() %>%
    magrittr::set_attr(x = .,
                       which = "Datenstand",
                       value = as.Date(sub(",.+", "", .$Datenstand[1]), format = "%d.%m.%Y")) %>%
    dplyr::mutate(Meldedatum = as.Date(gsub("\\s.+$", "", Meldedatum), format = "%Y/%m/%d"),
                  Altersgruppe = gsub("A", "", Altersgruppe),
                  Altersgruppe = ifelse(Altersgruppe == "unbekannt", "unknown", Altersgruppe)) %>%
    # join NUTS IDs and get rid of the Landkreis ID
    dplyr::left_join(lk_info, by = c("IdLandkreis" = "adm_unit")) %>%
    {if(any(is.na(.$lvl3))) stop("Error in mapping NUTS regions to Landkreise") else .} %>%
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

  saveRDS(cases, file = make_path(cache_dir, filename_cases))
  cat(paste0("Created case data set in '", make_path(cache_dir, filename_cases), "'."), fill = TRUE)

  rki_data %>%
    dplyr::select(-new_cases) %>%
    dplyr::rename(value = new_deaths) %>%
    saveRDS(., file = make_path(cache_dir, filename_deaths))
  cat(paste0("Created death data set in '", make_path(cache_dir, filename_deaths), "'."), fill = TRUE)

  return(cases)
}
