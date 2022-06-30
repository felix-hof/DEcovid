#' Get data on dominant SARS-CoV-2 variants in Germany
#' 
#' @description The function accesses data from \insertCite{DEcovid:VARdata;textual}{DEcovid} and calculates the relative prevalence for 
#' each of the variants that were dominant at some point in time. If this is desired, the data is extended to match the desired resolution. The
#' calculations within the functions are done by dividing the number of cases for each variant by the total number of people sequenced. If there
#' was no one sequenced within a specific week, the relative prevalence is assumed to be 0 for all virus variants.
#' 
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{list} whose elements are time series containing the relative prevalence for each of the SARS-CoV-2 variants that
#' were dominant at some point in time.
#' 
#' @export
#' 
#' @references 
#' \insertRef{DEcovid:VARdata}{DEcovid}
#'
#' @examples
#' variants <- get_variants()
#' variants <- get_variants(time_res = "daily", spat_res = 1, age_res = "no_age")
get_variants <- function(time_res = NULL,
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
  filename <- "variants.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 3, units = "days")

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
      dat <- get_variants_from_source(cache_dir = cache_dir, filename = filename)
    }
  }

  # aggregate if desired
  if(join){
    dims <- get_case_info(spat_res = 3, time_res = "daily", cache_dir = cache_dir)
    region <- dims$region
    age <- dims$age
    date <- dims$date
    earliest_obs <-
    dat <- lapply(dat, function(x){
      dates <- vapply(ISOweek::date2ISOweek(x$date), function(y){
        paste0(sub("\\d$", "", y), 1:7)
      }, character(7L))
      dim(dates) <- NULL
      dates <- ISOweek::ISOweek2date(dates)
      out <- dplyr::tibble(date = dates, value = rep(x$value, each = 7L))
      tidyr::expand_grid(age = age, date = date, region = region) %>%
        dplyr::left_join(y = out, by = "date") %>%
        summarise_data(time_res = time_res, spat_res = spat_res, age_res = age_res,
                       time_f = time_f_variants, spat_f = spat_f_variants, age_f = age_f_variants)
    })
  }

  return(dat)
}


#' Get a list with binary indicators of the dominant variant
#'
#' @template cache_dir
#' @param filename The name of the file where the time series of variants is stored.
#'
#' @return A \code{list} whose elements are time series indicating which variant was dominant in which week.
#'
#' @importFrom readr read_csv cols_only
#' @importFrom ISOweek ISOweek2date
#' @importFrom dplyr filter select %>% group_by summarise ungroup mutate rename arrange pull
#' @importFrom magrittr set_names
get_variants_from_source <- function(cache_dir, filename){

  vars <- readr::read_csv("https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv",
                          col_types = readr::cols_only(
                            country_code = "c",
                            year_week = "c",
                            source = "c",
                            number_sequenced = "i",
                            valid_denominator = "c",
                            variant = "c",
                            number_detections_variant = "i"
                          ),
                          progress = FALSE, trim_ws = TRUE, show_col_types = FALSE) %>%
    dplyr::filter(country_code == "DE") %>%
    dplyr::select(-country_code) %>%
    # implement the hierarchy
    dplyr::group_by(year_week, variant) %>%
    {if(any(.[["valid_denominator"]] == "Yes")) dplyr::filter(., valid_denominator == "Yes") else .} %>%
    {if(length(unique(.[["source"]])) > 1) dplyr::filter(., number_sequenced == number_sequenced[which.max(number_sequenced)]) else .} %>%
    dplyr::ungroup() %>%
    # calculate percentages (and return 0 if no one was sequenced that week)
    dplyr::mutate(percent_cases = ifelse(number_sequenced == 0L, 0L, number_detections_variant / number_sequenced)) %>%
    # if any error or weird stuff is going on: check the part above
    {
      # Throw message if non-valid denominator detected
      if(any(.[["valid_denominator"]] == "No")) message("Dataset variants: non-valid denominator detected.\n")
      .
    } %>%
    {
      # Get percentage of cases for each dominant variant
      # get all the variants that were dominant in at least one week
      dominant_variants <- dplyr::group_by(., year_week) %>%
        dplyr::summarise(variant = variant[which.max(percent_cases)], 
                         percent_cases = percent_cases[which.max(percent_cases)], .groups = "drop") %>%
        dplyr::filter(percent_cases != 0) %>% 
        dplyr::pull(variant) %>%
        unique()
      # convert week to date object
      .$year_week <- ISOweek::ISOweek2date(vapply(strsplit(.$year_week, split = "-", fixed = TRUE),
                                                     function(x) paste0(x[1], "-W", x[2], "-4"), character(1L)))
      # get the entire date series
      unique_dates <- sort(unique(.$year_week))
      if(!all(diff(unique_dates) == 7)) stop("There are days missing in the creation of the virus variants data.")
      out <- lapply(dominant_variants, function(x){
        .[] %>%
          dplyr::filter(variant == x) %>%
          dplyr::select(year_week, percent_cases) %>%
          dplyr::right_join(y = dplyr::tibble(year_week = unique_dates), by = "year_week") %>%
          dplyr::rename(date = year_week, value = percent_cases) %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(value = ifelse(is.na(value) & date < date[which(!is.na(value))[1L]], 0, value))
      }) %>%
        magrittr::set_names(dominant_variants)
      out
    }

  saveRDS(vars, file = make_path(cache_dir, filename))

  return(vars)
}
