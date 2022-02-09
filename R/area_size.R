#' Get area_size from cache or source
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{lvl3} and \code{value}. The column \code{value} contains the
#' area size for each NUTS 3 region.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @importFrom tidyr expand_grid
#'
#' @export
#'
#' @examples
#' area_size <- get_area_size()
#' area_size <- get_area_size(time_res = "daily", spat_res = 1, age_res = "no_age")
#'
get_area_size <- function(time_res = NULL,
                          spat_res = NULL,
                          age_res = NULL,
                          cache_dir = NULL){

  # Check inputs
  join <- check_res_args(time_res = time_res,
                         spat_res = spat_res,
                         age_res = age_res)

  # set parameters for cacheing
  filename <- "area_size.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 90, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_area_size_from_source(cache_dir, filename)
  }

  # aggregate if desired
  if(join){
    dims <- get_case_info(spat_res = 3, time_res = "daily", cache_dir = cache_dir)
    region <- dims$region
    age <- dims$age
    date <- dims$date
    dat <- tidyr::expand_grid(age = age, date = date, region = region) %>%
      dplyr::left_join(y = dat, by = "region") %>%
      summarise_data(time_res = time_res, spat_res = spat_res, age_res = age_res,
                     time_f = time_f_area_size, spat_f = spat_f_area_size, age_f = age_f_area_size)
  }

  return(dat)
}

#' Get area size from Eurostat
#'
#' @template cache_dir
#' @param  filename The name of the file where the area size data is stored.
#'
#' @return A \code{tibble} with columns \code{lvl3} and \code{value} (contains area sizes).
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter select mutate rename arrange
#'
get_area_size_from_source <- function(cache_dir, filename){

  # Get other stuff
  ref <- nuts_table(cache_dir) %>%
    dplyr::select(lvl3, lvl3_name) %>%
    dplyr::distinct()

  # Set parameters
  dataset <- "reg_area3"
  args <- list(geoLevel = paste0("nuts", 3),
               filterNonGeo = "1",
               precision = "1",
               unit = "KM2",
               landuse = "TOTAL",
               shortlabel = "1",
               time = "2016")

  # Get data (and add DEG0N to DEG0P)
  dat <- eurostat_query(dataset = dataset, args = args) %>%
    dplyr::select(-landuse, -unit, -time, -index) %>%
    dplyr::filter(grepl("^DE.{3}", geo)) %>%
    dplyr::mutate(geo = replace(geo, geo == "DEG0N", "DEG0P")) %>%
    dplyr::group_by(geo) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::rename(region = geo) %>%
    dplyr::arrange(region) %>%
    dplyr::filter(region %in% ref$lvl3)

  if(length(setdiff(dat$region, ref$lvl3)) != 0 || length(setdiff(ref$lvl3, dat$region)) != 0){
    stop("The NUTS regions from Eurostat and the ones we use in the thesis are not the same.", fill = TRUE)
  }

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
