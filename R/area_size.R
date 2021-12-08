#' Get area_size from cache or source
#'
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{lvl3} and \code{value}. The column \code{value} contains the
#' area size for each NUTS 3 region.
#' @export
#'
#' @examples
#' area_size <- get_area_size()
#'
get_area_size <- function(cache_dir = NULL){

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
    dplyr::rename(lvl3 = geo) %>%
    dplyr::arrange(lvl3) %>%
    dplyr::filter(lvl3 %in% ref$lvl3)

  if(length(setdiff(dat$lvl3, ref$lvl3)) != 0 || length(setdiff(ref$lvl3, dat$lvl3)) != 0){
    stop("The NUTS regions from Eurostat and the ones we use in the thesis are not the same.", fill = TRUE)
  }

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
