# get_nb_matrix <- function(geoms, age_groups, cache_dir = NULL){
#
#   # check input
#   if(!(nuts_level %in% 0:3) || length(nuts_level) != 1) stop("Argument 'nuts_level' must be one of c(0, 1, 2, 3).")
#   if(!(with_age %in% c(TRUE, FALSE)) || length(with_age) != 1) stop("Argument 'with_age' must be either TRUE or FALSE.")
#   cache_dir <- get_cache_dir(cache_dir)
#
#   # Set parameters
#   nb_pattern <- "F***1****" # Use "F***T****" if a touch point is enough for two regions to be nbs
#
#   # load level 3 regions and age groups
#   file <- make_path(cache_dir, "case_regions_3.rds")
#   if(!file.exists(file)){
#     get_cases(cache_dir = cache_dir)
#   }
#   regions <- readRDS(file)
#   file <- make_path(cache_dir, "agegroups.rds")
#   if(!file.exists(file)){
#     get_cases(cache_dir = cache_dir)
#   }
#   agegroups <- readRDS(file)
#
#
#
# }

# geoms <- get_geoms()[[4]]
# agegroups <- readRDS(make_path(tempdir(), "agegroups.rds"))
# regions <- readRDS(make_path(tempdir(), "case_regions_3.rds"))

# This function only needs geoms and agegroups. The checking whether geoms
# are actually correct is done somewhere else



#' Compute neighbourhood order matrices stratified by region and age group
#'
#' @param regions A \code{character} vector containing all region names.
#' @param agegroups A \code{character} vector containing all age group labels in increasing order.
#' @param geoms An object of class \code{sf} with at least a column named \code{region} that contains the unit names and a geometry column.
#' @param nb_pattern The neighbourhood \code{pattern} that is passed to \code{\link[sf]{st_relate}}.
#' @param cache_dir The directory where the output matrix is saved.
#'
#' @return
#' @export
#'
#' @examples
compute_nb_matrix <- function(regions, agegroups, geoms, nb_pattern, cache_dir = NULL){

  # Check regions
  if(!all(regions %in% geoms$region)) stop("Not all regions are present in geoms.")
  if(!all(geoms$region %in% regions)) stop("Some geoms are not present on the passed regions.")

  # age groups
  ## age group matrix (relies on age groups being in correct order)
  age_mat <- stats::toeplitz(seq_along(agegroups) - 1L)

  # Compute the actual matrix
  nb_list <- sf::st_relate(geoms, pattern = nb_pattern)
  combined_mat <- do.call("rbind", parallel::mclapply(seq_along(nb_list), function(x){
    ## spatial neighbourhood matrix
    out <- rep(NA_integer_, length(nb_list)); out[x] <- 0L
    current_nb <- nb_list[[x]]
    order <- 1L
    while(any(is.na(out))){
      out[current_nb] <- order
      current_nb <- unique(unlist(nb_list[current_nb]))
      current_nb <- current_nb[is.na(out[current_nb])]
      order <- order + 1L
    }
    ## combine with age matrix
    # at this point "out" is the row of the neighbourhood order matrix
    # now replace each element of this vector by age_mat + vector_element
    test <- do.call("cbind", lapply(out, "+", y = age_mat))
  },
  mc.cores = parallel::detectCores() - 1L,
  mc.preschedule = FALSE,
  mc.cleanup = TRUE))

  # set row and column names
  nms <- tidyr::expand_grid(region = geoms$region, age = agegroups)
  if(!all(nms$region[seq_along(agegroups)] == nms$region[1])) stop("Row and column names are wrong. Regions should stay constant while agegroups should vary.")
  colnames(combined_mat) <- rownames(combined_mat) <- paste0(nms$region, ".", nms$age)

  # save object
  saveRDS(combined_mat, file = filename)

  return(combined_mat)
}
