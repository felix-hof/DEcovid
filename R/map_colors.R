#' Get map color indices
#'
#' @param geoms A single object of class \code{sf} or a list whose elements are of class \code{sf}.
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{region} and \code{color} (contains color indices).
#'
#' @details Under the hood, this function uses the function \code{getColoring}
#' from the package \href{https://github.com/hunzikp/MapColoring}{\code{MapColoring}}.
#'
get_map_colors <- function(geoms, cache_dir = NULL){

  # set neighbourhood pattern used for adjacency matrices
  nb_pattern <- "F***1****" # Use "F***T****" if a touch point is enough for two regions to be nbs

  # set parameters for cacheing
  filename <- "map_colors.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 120, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_map_colors_from_source(geoms = geoms,
                                      nb_pattern = nb_pattern, cache_dir = cache_dir,
                                      filename = filename)
  }

  return(dat)
}

#' Get map color indices
#'
#' @param geoms A single object of class \code{sf} or a list whose elements are of class \code{sf}.
#' @param nb_pattern A \code{character} vector of length one. Must contain a valid DE9-IM string describing the
#' relationship of \code{geoms}. This argument will be passed as the \code{pattern} argument to
#'  \code{\link[sf:st_relate]{st_relate()}}.
#' @template cache_dir
#' @param  filename The name of the file where the color data is stored.
#'
#' @return A \code{tibble} with columns \code{region} and \code{color} (contains color indices).
#'
#' @details Under the hood, this function uses the function \code{getColoring}
#' from the package \href{https://github.com/hunzikp/MapColoring}{\code{MapColoring}}.
#'
#' @importFrom sf st_relate
#' @importFrom MapColoring getColoring
#' @importFrom dplyr tibble
#'
get_map_colors_from_source <- function(geoms, nb_pattern, cache_dir, filename){

  # convert to list
  if("sf" %in% class(geoms)){
    geoms <- list(geoms)
  }

  # Input checks
  # check whether all objects are of class sf,
  # check whether all objects have 2 columns,
  # check whether all objects have a geometry column,
  # check whether name column is unique
  check <- vapply(geoms, function(x){
    class <- tryCatch({"sf" %in% class(x)},
                      error = function(cond){FALSE})
    cols <- tryCatch({isTRUE(ncol(x) == 2)},
             error = function(cond){FALSE})
    geom_col <- tryCatch({!is.null(attributes(x)$sf_column)},
             error = function(cond){FALSE})
    unique_names <- tryCatch({length(unique(x[[which(colnames(x) != attributes(x)$sf_column)]])) == nrow(x)},
                             error = function(cond){FALSE})
    return(c(class, cols, geom_col, unique_names))
  }, logical(4L))
  errors <- c("- The object or list passed as 'geoms' is/contains at least one object that is not of class 'sf'.",
              "- The 'geoms' object(s) must have only two columns.",
              "- At least one of the 'geoms' object(s) does not have a geometry column.",
              "- At least one of the 'geoms' object(s) does not have a unique name column.")
  msg <- errors[apply(check, 1, function(x){any(!x)})]
  if(length(msg) != 0L){
    stop(paste0("The 'geoms' object did not pass input checks. The following errors were found:\n",
                paste0(msg, collapse = "\n")))
  }

  # get which one is the name column
  name_cols <- vapply(geoms, function(x){colnames(x)[which(colnames(x) != attributes(x)$sf_column)]}, character(1L))

  # Get neighbourhood matrices
  dat <- lapply(seq_along(geoms), function(x){
    adj_mat <- sf::st_relate(geoms[[x]], pattern = nb_pattern, sparse = FALSE)
    return(
      dplyr::tibble(region = geoms[[x]][[name_cols[x]]],
                    color = MapColoring::getColoring(adj_mat))
    )
  })

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
