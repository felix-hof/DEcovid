#' Compute neighbourhood order matrices stratified by region and/or age group
#'
#' @param by A \code{character} vector. Contains one or both of \code{c("age", "region")}.
#' @param nuts_level A \code{character} vector of length 1 indicating the desired NUTS level of spatial regions. The argument is ignored if
#' \code{by = "age"}.
#' @template cache_dir
#'
#' @return A \code{matrix} containing the neighbourhood order of the units defined via the \code{by} argument.
#' @export
#' @references 
#' \insertRef{DEcovid:ESgeoms}{DEcovid}
#' @examples
#' nb_regions_agegroups <- get_nb_matrix(nuts_level = 1)
#' nb_regions <- get_nb_matrix(by = "region", nuts_level = 1)
#'
get_nb_matrix <- function(by = c("age", "region"), nuts_level = 3, cache_dir = NULL){


  if(!(nuts_level %in% 0:3) || length(nuts_level) != 1) stop("Argument 'nuts_level' must be one of c(0, 1, 2, 3).")
  if(length(by) == 1L && by == "age") stop("Use function 'get_contacts' for contact matrices.")
  if(length(by) == 1 && by == "region" && nuts_level == 0){
    out <- as.matrix(1L)
    rownames(out) <- colnames(out) <- "DE"
    return(out)
  }
  if(!all(by %in% c("age", "region"))) stop("The argument 'by' must be a subset of c(\"age\", \"region\")")
  by <- match.arg(by, several.ok = TRUE)
  cache_dir <- get_cache_dir(cache_dir)

  # Set parameters
  nb_pattern <- "F***1****" # Use "F***T****" if a touch point is enough for two regions to be nbs
  filename <- paste0("nb_order_mat_", nuts_level, ".rds")

  # Check whether file exists
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 90, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    # load necessary data
    geoms <- get_geoms(cache_dir = cache_dir)[[nuts_level + 1]]
    if(file.exists(make_path(cache_dir, paste0("case_regions_", nuts_level, ".rds")))){
      regions <- readRDS(make_path(cache_dir, paste0("case_regions_", nuts_level, ".rds")))
    } else {
      get_cases(cache_dir = cache_dir)
      regions <- readRDS(make_path(cache_dir, paste0("case_regions_", nuts_level, ".rds")))
    }
    if(file.exists(make_path(cache_dir, "agegroups.rds"))){
      agegroups <- readRDS(make_path(cache_dir, "agegroups.rds"))
    } else {
      get_cases(cache_dir = cache_dir)
      regions <- readRDS(make_path(cache_dir, "agegroups.rds"))
    }
    # calculate matrix
    dat <- compute_nb_matrix(regions = regions, agegroups = agegroups,
                             geoms = geoms, nb_pattern = nb_pattern,
                             save_location = make_path(cache_dir, filename))
  }

  # return matrix as desired
  if(!(length(by) == 2L)){
    if(by == "region"){
      idx <- grepl(sub("^.+\\.(.+)$", "\\1", colnames(dat)[1]), colnames(dat))
      dat <- dat[idx, idx]
      colnames(dat) <- rownames(dat) <- gsub("^(.+)\\..+$", "\\1", colnames(dat))
    } else if(by == "age") {
      agegroups <- unique(gsub("^.+\\.(.+)$", "\\1", colnames(dat)))
      dat <- dat[seq_along(agegroups), seq_along(agegroups)]
      colnames(dat) <- rownames(dat) <- agegroups
    } else {
      stop("Check function. This should not be possible.")
    }
  }

  return(dat)
}

# geoms <- get_geoms()[[4]]
# agegroups <- readRDS(make_path(tempdir(), "agegroups.rds"))
# regions <- readRDS(make_path(tempdir(), "case_regions_3.rds"))

#' Compute neighbourhood order matrices stratified by region and age group
#'
#' @param regions A \code{character} vector containing all region names.
#' @param agegroups A \code{character} vector containing all age group labels in increasing order.
#' @param geoms An object of class \code{sf} with at least a column named \code{region} that contains the unit names and a geometry column.
#' @param nb_pattern The neighbourhood \code{pattern} that is passed to \code{\link[sf]{st_relate}}.
#' @param save_location A filesystem path where the output matrix is saved.
#'
#' @return A matrix containing the neighbourhood order between the units
#'
#' @importFrom sf st_relate
#' @importFrom parallel mclapply detectCores
#' @importFrom tidyr expand_grid
#' @noRd
compute_nb_matrix <- function(regions, agegroups, geoms, nb_pattern, save_location = NULL){

  ## check regions
  if(!all(regions %in% geoms$region)) stop("Not all regions are present in geoms.")
  if(!all(geoms$region %in% regions)) stop("Some geoms are not present on the passed regions.")

  # age groups
  ## age group matrix (relies on age groups being in correct order)
  age_mat <- matrix(0L, nrow = length(agegroups), ncol = length(agegroups))

  # avoid error in R CMD CHECK
  # code taken from https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    num_workers <- 2L
  } else {
    # use all cores in devtools::test()
    num_workers <- parallel::detectCores() - 1
  }

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
  mc.cores = num_workers,
  mc.preschedule = FALSE,
  mc.cleanup = TRUE))

  # set row and column names
  nms <- tidyr::expand_grid(region = geoms$region, age = agegroups)
  if(!all(nms$region[seq_along(agegroups)] == nms$region[1])) stop("Row and column names are wrong. Regions should stay constant while agegroups should vary.")
  colnames(combined_mat) <- rownames(combined_mat) <- paste0(nms$region, ".", nms$age)

  # save object
  saveRDS(combined_mat, file = save_location)

  return(combined_mat)
}

#' Get a contact matrix for Germany
#' 
#' @description The contact matrix is sourced from \insertCite{DEcovid:MOBS;textual}{DEcovid} and aggregated using the population data available from
#' \insertRef{DEcovid:ESpoponeyear}{DEcovid}.
#'
#' @template cache_dir 
#' @template enforce_cache 
#'
#' @return A contact matrix aggregated to the age groups defined in the case data set available through \code{\link[DEcovid]{get_cases}}.
#' @export
#' @references 
#' \insertRef{DEcovid:MOBS}{DEcovid}
#'
#' @examples
#' C <- get_c_matrix()
#' 
get_c_matrix <- function(cache_dir = NULL,
                         enforce_cache = FALSE){
  
  # set parameters for cacheing
  filename <- "contact_matrix.rds"
  cache_dir <- get_cache_dir(cache_dir)
  
  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 90, units = "days")
  
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
      dat <- get_c_matrix_from_source(filename, cache_dir, enforce_cache = enforce_cache)
    }
  }
  
  return(dat)
}


#' Get contact matrix from source
#'
#' @param filename The name of the file saved in cache
#' @template cache_dir 
#' @template enforce_cache 
#'
#' @return A contact matrix aggregated to the age groups defined in the case data set available through \code{\link[DEcovid]{get_cases}}.
#' @noRd
#' @importFrom hhh4contacts aggregateC
#' @importFrom stats setNames
#' @importFrom utils read.csv
get_c_matrix_from_source <- function(filename, cache_dir, enforce_cache){
  
  # get matrix
  C <- "https://raw.githubusercontent.com/mobs-lab/mixing-patterns/main/data/contact_matrices/Germany_country_level_M_overall_contact_matrix_85.csv" %>% 
    read.csv(header = FALSE) %>% 
    as.matrix()
  # label it
  row_col_names <- ifelse(nchar(0 : (ncol(C) - 1)) == 1,
                          paste0(0, 0 : (ncol(C) - 1)),
                          (0 : (ncol(C) - 1)))
  row_col_names <- paste0(row_col_names, "-", row_col_names)
  row_col_names[length(row_col_names)] <- sub("-.+", "+", row_col_names[length(row_col_names)])
  dimnames(C) <- list(participant = row_col_names, contact = row_col_names)
  
  # get population weights for the current age groups
  pop <- get_one_year_population(type = "raw", cache_dir = cache_dir, enforce_cache = enforce_cache) %>% 
    dplyr::group_by(age) %>% 
    dplyr::summarise(values = sum(values)) %>% 
    dplyr::mutate(age_lab = ifelse(nchar(age) == 1L, paste0(0, age), age),
                  age_lab = paste0(age_lab, "-", age_lab),
                  age_lab = ifelse(age >= ncol(C) - 1, paste0(ncol(C) - 1, "+"), age_lab)) %>% 
    dplyr::group_by(age_lab) %>% 
    dplyr::summarise(values = sum(values))
  
  # get the age groups
  ages <- unique(get_population(cache_dir = cache_dir, enforce_cache = enforce_cache)$age)
  
  # make grouping variable
  grouping <- vapply(ages, function(x){
    # get start and end age of the group based on the location of the dash
    # in the label (which is defined based on the object brk from setup.R)
    start_end <- unlist(strsplit(x, "-"))
    if(length(start_end) == 1){
      start_end <- c(start_end, nrow(C) - 1)
    }
    # convert to numeric
    start_end <- as.numeric(gsub("\\D", "", start_end))
    # return length of the sequence from start to end
    return(length(seq(start_end[1], start_end[2], by = 1)))
  }, numeric(1L), USE.NAMES = FALSE)
  
  # Construct weights
  w <- setNames(pop$values / sum(pop$values), pop$age_lab)
  
  # Aggregate matrix
  C <- hhh4contacts::aggregateC(C, weights = w, grouping = grouping)
  
  # save it
  saveRDS(C, file = make_path(cache_dir, filename))
  
  C
  
}
