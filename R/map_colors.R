#' Color polygon geometries such that no element has the same color as all of its neighbours.
#' 
#' @details This function provides an efficient implementation of the DSATUR-algorithm developed by \insertCite{DEcovid:dsatur;textual}{DEcovid}.
#' 
#' @param geoms A single object of class \code{sf} or a list whose elements are of class \code{sf}.
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{tibble} with columns \code{region} and \code{color} (contains color indices).
#'
#' @references 
#' \insertRef{DEcovid:dsatur}{DEcovid}
#' 
get_map_colors <- function(geoms, cache_dir = NULL, enforce_cache = FALSE){

  check_enforce_cache(enforce_cache = enforce_cache)

  # set neighbourhood pattern used for adjacency matrices
  nb_pattern <- "F***1****" # Use "F***T****" if a touch point is enough for two regions to be nbs

  # set parameters for cacheing
  filename <- "map_colors.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 120, units = "days")

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
      dat <- get_map_colors_from_source(geoms = geoms,
                                        nb_pattern = nb_pattern, cache_dir = cache_dir,
                                        filename = filename)
    }
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
#'
#' @importFrom sf st_relate
#' @importFrom dplyr tibble
#' @noRd
get_map_colors_from_source <- function(geoms, nb_pattern, cache_dir, filename){

  # convert to list
  if(inherits(geoms, "sf")){
    geoms <- list(geoms)
  }

  # Input checks
  # check whether all objects are of class sf,
  # check whether all objects have 2 columns,
  # check whether all objects have a geometry column,
  # check whether name column is unique
  check <- vapply(geoms, function(x){
    class <- tryCatch({inherits(x, "sf")},
                      error = function(cond){FALSE})
    cols <- tryCatch({isTRUE(ncol(x) == 2L)},
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
                    color = factor(dsatur(adj_mat)))
    )
  })

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}

#' Apply the Dsatur algorithm to an adjacency matrix
#'
#' @description The Dsatur algorithm is described in: \cr
#' Brélaz, Daniel (1979). New methods to color the vertices of a graph. \cr
#' Communications of the ACM 22(4) pp. 251-256. DOI: https://doi.org/10.1145/359094.359101. \cr
#' \cr
#' The algorithm is used to color units in a map with a minimal amount of colors such that there are
#' never two adjacent units with the same color.
#'
#' @param adj_mat A symmetric adjacency matrix of either \code{integer} or \code{logical} type. Numeric matrices
#' will internally be forced to logical type by \code{as.logical}. Adjacencies are expected
#' to be binary (\code{c(1, 0)} or \code{c(TRUE, FALSE)}).
#'
#' @return An integer vector containing
#' @noRd
#'
dsatur <- function(adj_mat){

  if(!inherits(adj_mat, "matrix")) stop("Argument 'adj_mat' must be a matrix.")
  if(!isTRUE(all.equal(t(adj_mat), adj_mat))) stop("The matrix 'adj_mat' must be symmetric.")
  if(!(typeof(adj_mat) %in% c("integer", "logical"))) stop("The matrix 'adj_mat' must be of type integer or logical.")
  if(any(is.na(adj_mat))) stop("The matrix 'adj_mat' must not contain NAs.")
  # Convert adjacency matrix to logical and make sure a vertex cannot be its own neighbour
  if(typeof(adj_mat) != "logical") adj_mat <- as.logical(adj_mat)
  diag(adj_mat) <- FALSE

  ## By defining these functions in here, we can actually use
  # Function that returns the neighbours of a given vertex
  get_neighbours <- function(vertex) which(adj_mat[vertex, ])
  # Function that calculates the saturation of a given vertex
  get_saturation <- function(vertex) length(unique(colors[get_neighbours(vertex)]))

  # Get number of vertices
  n <- nrow(adj_mat)
  # If n = 1, return color "1"
  if(n == 1L) return(1L)
  # Make an index vector for the vertices, and a vector to keep track of uncolored vertices
  indices <- uncolored_vertices <- 1L:n
  # Initialise saturation, colors and a color counter
  # (saturation of a vertex is the number of different colors within the level 1 neighbourhood)
  colors <- saturation <- rep.int(0L, times = n)
  # Calculate number of neighbours for each vertex
  degrees <- rowSums(adj_mat)
  # Get one of the vertices with maximum degree (used as the initial vertex)
  idx_max_degree <- which.max(degrees)[1L]
  # Color initial vertex and remove it from the list of uncolored vertices
  colors[idx_max_degree] <- 1L
  uncolored_vertices <- uncolored_vertices[-idx_max_degree]
  # Increase saturation of neighbours of the now colored vertex
  saturation[get_neighbours(idx_max_degree)] <- 1L
  # Initialise a color counter
  color_count <- 1L

  #loop_count <- 0L
  repeat{
    #loop_count <- loop_count + 1L
    # choose vertex with maximum saturation (if tied, get one of maximal degree)
    sat_uncolored <- saturation[uncolored_vertices]
    current_vertex <- uncolored_vertices[which(sat_uncolored == max(sat_uncolored))]
    if(length(current_vertex) != 1L)
      current_vertex <- current_vertex[which.max(degrees[current_vertex])]
    # find lowest possible color and increase color counter if necessary
    possible_colors <- 1L:(color_count + 1L)
    cond <- !(possible_colors %in% colors[get_neighbours(current_vertex)])
    lowest <- possible_colors[cond][1L]
    if(lowest == rev(possible_colors)[1L])
      color_count <- color_count + 1L
    # color vertex with lowest possible color
    colors[current_vertex] <- lowest
    # remove current vertex from the uncolored vertices
    uncolored_vertices <- uncolored_vertices[uncolored_vertices != current_vertex]
    # break loop if no uncolored vertices
    if(length(uncolored_vertices) == 0) break
    # recalculate saturation
    saturation <- vapply(X = indices, FUN = get_saturation, FUN.VALUE = integer(1L))
  }
  colors
}


