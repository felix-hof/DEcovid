#' Calculate the log-population for each NUTS-region
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{tibble} containing the log-population at the desired resolution.
#' @export
#'
#' @examples
#' grav <- get_gravity()
#' grav <- get_gravity(time_res = "weekly", spat_res = 1L, age_res = "no_age")
#'
get_gravity <- function(time_res = NULL,
                        spat_res = NULL,
                        age_res = NULL,
                        cache_dir = NULL,
                        enforce_cache = FALSE){

  pop <- get_population(time_res = time_res,
                        spat_res = spat_res,
                        age_res = age_res,
                        cache_dir = cache_dir,
                        enforce_cache = enforce_cache)
  pop$value <- log(pop$value)
  pop
}
