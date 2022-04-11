#' Calculate the population density for each NUTS-region
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{tibble} containing population density at the desired resolution.
#' @export
#'
#' @examples
#' dens <- get_density()
#' dens <- get_density(time_res = "weekly", spat_res = 1L, age_res = "no_age")
#'
get_density <- function(time_res = NULL,
                        spat_res = NULL,
                        age_res = NULL,
                        cache_dir = NULL,
                        enforce_cache = FALSE){

  # Check inputs
  join <- check_res_args(time_res = time_res,
                         spat_res = spat_res,
                         age_res = age_res)
  check_enforce_cache(enforce_cache = enforce_cache)

  # get cache_directory
  cache_dir <- get_cache_dir(cache_dir = cache_dir)

  # set default spatial resolution to NUTS level 3
  if(is.null(spat_res)) spat_res <- 3L

  # calculate density from area and population
  dat <- lapply(list(area = get_area_size(enforce_cache = enforce_cache),
                     population = get_population(enforce_cache = enforce_cache)),
                function(x){
                  x %>%
                    dplyr::mutate(region = substr(region, 1, spat_res + 2L)) %>%
                    dplyr::group_by(region) %>%
                    dplyr::summarise(value = sum(value))
                }) %>%
    dplyr::left_join(x = .[[1L]], y = .[[2L]], by = "region", suffix = c(".area", ".pop")) %>%
    dplyr::mutate(value = value.pop / value.area) %>%
    dplyr::select(region, value)

  # aggregate if desired
  if(join){
    dims <- get_case_info(spat_res = spat_res, time_res = time_res, cache_dir = cache_dir)
    region <- dims$region
    age <- if(age_res == "age") dims$age else "total"
    date <- dims$date
    dat <- tidyr::expand_grid(age = age, date = date, region = region) %>%
      dplyr::left_join(y = dat, by = "region")
  }

  return(dat)
}
