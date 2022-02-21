#' Get a list with binary indicators of the dominant variant
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#'
#' @return A \code{list} whose elements are time series indicating which variant was dominant in which week.
#' @export
#'
#' @examples
#' variants <- get_variants()
#' variants <- get_variants(time_res = "daily", spat_res = 1, age_res = "no_age")
get_variants <- function(time_res = NULL,
                         spat_res = NULL,
                         age_res = NULL,
                         cache_dir = NULL){

  # Check inputs
  join <- check_res_args(time_res = time_res,
                         spat_res = spat_res,
                         age_res = age_res)

  # set parameters for cacheing
  filename <- "variants.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 3, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_variants_from_source(cache_dir = cache_dir, filename = filename)
  }

  # aggregate if desired
  if(join){
    dims <- get_case_info(spat_res = 3, time_res = "daily", cache_dir = cache_dir)
    region <- dims$region
    age <- dims$age
    date <- dims$date
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
#' @importFrom dplyr filter select %>% group_by ungroup mutate rename arrange pull
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
    dplyr::group_by(year_week) %>%
    {if(any(.[["valid_denominator"]] == "Yes")) dplyr::filter(., valid_denominator == "Yes") else .} %>%
    {if(length(unique(.[["source"]])) > 1) dplyr::filter(., number_sequenced == number_sequenced[which.max(number_sequenced)]) else .} %>%
    # get the dominant variant
    dplyr::filter(number_detections_variant == number_detections_variant[which.max(number_detections_variant)]) %>%
    # set rows to NA if no one was sequenced that week
    dplyr::mutate(number_detections_variant = ifelse(all(number_detections_variant == 0L), NA_integer_, number_detections_variant)) %>%
    dplyr::ungroup() %>%
    # filter out rows where no one was sequenced
    dplyr::filter(!is.na(number_detections_variant)) %>%
    # if any error or weird stuff is going on: check the part above
    {
      # Throw message if non-valid denominator detected
      if(any(.[["valid_denominator"]] == "No")) message("Dataset variants: non-valid denominator detected.\n")
      # Throw warning in case of ties
      if(length(.[["year_week"]]) != length(unique(.[["year_week"]]))) warning("Dataset variants: There are ties within a week.\n")
      .
    } %>%
    dplyr::select(year_week, variant) %>%
    # convert date object
    dplyr::mutate(year_week = gsub("^(\\d{4})-(\\d{2})", "\\1-W\\2-4", year_week)) %>% # Use Thursday as reference (As I cannot find the day this is updated)
    dplyr::mutate(year_week = ISOweek::ISOweek2date(year_week)) %>%
    # prettify
    dplyr::rename(date = year_week, value = variant) %>%
    dplyr::arrange(date)

  variants <- vars %>% dplyr::filter(!is.na(value)) %>% dplyr::pull(value) %>% unique()
  vars <- lapply(variants, function(x){
    vars %>%
      dplyr::mutate(value = dplyr::case_when(is.na(value == x) ~ 0L,
                                             value != x ~ 0L,
                                             value == x ~ 1L))
  }) %>%
    magrittr::set_names(variants)

  saveRDS(vars, file = make_path(cache_dir, filename))

}
