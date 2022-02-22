#' Make border indicators from geometries provided by Eurostat
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#'
#' @return A \code{list} of length 10. Each list element corresponds to a specific neighbour country or a general
#' spatial location at the country border. Each list element is a \code{tibble} with two columns \code{lvl3} and \code{value}. The
#' former indicates the respective NUTS 3 region whereas the latter indicates if the given region borders the specific country.
#' @export
#'
#' @examples
#' bounds <- get_boundary_inds()
#'
get_boundary_inds <- function(time_res = NULL,
                              spat_res = NULL,
                              age_res = NULL,
                              cache_dir = NULL){

  # Check inputs
  join <- check_res_args(time_res = time_res,
                         spat_res = spat_res,
                         age_res = age_res)

  # set parameters for cacheing
  filename <- "boundary_inds.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 90, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_boundary_inds_from_source(cache_dir, filename)
  }

  # aggregate if desired
  if(join){
    dims <- get_case_info(spat_res = 3, time_res = "daily", cache_dir = cache_dir)
    region <- dims$region
    age <- dims$age
    date <- dims$date
    dat <- lapply(dat, function(x){
      tidyr::expand_grid(age = age, date = date, region = region) %>%
        dplyr::left_join(y = x, by = "region") %>%
        summarise_data(time_res = time_res, spat_res = spat_res, age_res = age_res,
                       time_f = time_f_border_inds, spat_f = spat_f_border_inds, age_f = age_f_border_inds)
    })
  }

  return(dat)
}

#' Make border indicators from geometries provided by Eurostat
#'
#' @template cache_dir
#' @param  filename The name of the file that the border indicators are stored in.
#'
#' @return A \code{list} of length 10. Each list element corresponds to a specific neighbour country or a general
#' spatial location at the country border. Each list element is a \code{tibble} with two columns \code{lvl3} and \code{value}. The
#' former indicates the respective NUTS 3 region whereas the latter indicates if the given region borders the specific country.
#'
#' @importFrom dplyr %>% filter arrange tibble mutate across rename
#' @importFrom sf st_relate
#' @importFrom purrr map_dfc map_int
#' @importFrom rlang :=
#' @importFrom tidyr everything
#'
get_boundary_inds_from_source <- function(cache_dir, filename){

  # other definitions
  nb_pattern <- "F***1****" # Use "F***T****" if a point where geoms touch is enough for two regions to be neighbours

  # get geometries and filter them
  de_geom <- get_geoms()[[4]] %>%
    dplyr::filter(nchar(region) == 5) %>%
    dplyr::arrange(region)
  neigh_geom <- get_geoms()[[5]] %>%
    dplyr::arrange(region)

  # get which region neighbours what country
  nb_list <- sf::st_relate(de_geom,
                           neigh_geom,
                           pattern = nb_pattern)

  # make covariates (select each neighbour country and each lvl3 region, then join join_object)
  dat <- purrr::map_dfc(seq_along(neigh_geom$region), function(x){
    purrr::map_int(nb_list, function(y){
      if(x %in% y) 1L else 0L
    }) %>%
      {dplyr::tibble(!!as.symbol(neigh_geom$region[x]) := .)}
  }) %>%
    dplyr::mutate(ALL = ifelse(rowSums(dplyr::across(tidyr::everything())) > 0L, 1L, 0L),
                  region = de_geom$region) %>%
    {
      namecol <- grep("region", colnames(.))
      lapply(seq_along(.)[-namecol], function(x){
        .[, c(x, namecol)] %>%
          dplyr::rename(value = 1)
      })
    }
  names(dat) <- c(neigh_geom$region, "ALL")

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
