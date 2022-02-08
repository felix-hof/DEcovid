#' Get geometries for the thesis
#'
#' @template cache_dir
#'
#' @return A \code{list} of length 5. All elements of this list are of class \code{sf}. The first four elements
#' contain geometries of German NUTS regions on levels 0-3, whereas the fifth elements contains
#' geometries of Germany's neighbour countries.
#' @export
#'
#' @examples
#' geoms <- get_geoms()
get_geoms <- function(cache_dir = NULL){

  # set parameters for cacheing
  filename <- "geoms.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  #cat(paste(ls(), collapse = ", "))
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 60, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- process_shapefiles(cache_dir = cache_dir, filename = filename)
  }

  # add color indices
  colors <- get_map_colors(geoms = dat, cache_dir = cache_dir)
  dat <- lapply(seq_along(dat), function(x){
    dplyr::left_join(x = dat[[x]], y = colors[[x]], by = "region")
  })

  return(dat)
}

#' Get geometries of German NUTS units and neighbouring countries as \code{sf} object
#'
#' @template cache_dir
#' @param filename The filename under which these geometries should be stored in \code{cache_dir}
#'
#' @return The function returns \code{0} invisibly.
#'
#' @importFrom sf st_read st_union
#' @importFrom dplyr select filter %>% arrange
#'
process_shapefiles <- function(cache_dir, filename){

  # Which shapefile to use
  shapefile_dir <- "shapefiles"
  shp_to_use <- "NUTS_RG_01M_2021_3035"

  # Find out whether to use cached shapefiles
  #cache_dir <- get_cache_dir(cache_dir)
  dir_path <- make_path(cache_dir, shapefile_dir)
  make_new <- TRUE
  if(dir.exists(dir_path)){
    make_new <- needs_update(dir_path, 60, "days")
  }

  # Download shapefiles if necessary
  if(make_new){
    get_shapefiles_from_source(cache_dir)
  }

  # read data and process it
  areas <- sf::st_read(dsn = make_path(cache_dir, shapefile_dir, shp_to_use),
                       layer = shp_to_use, quiet = TRUE) %>%
    dplyr::select(NUTS_ID, geometry) %>%
    # remove overseas territories and Corse from France
    {
      .$geometry[.$NUTS_ID == "FR"] <- dplyr::filter(., nchar(NUTS_ID) == 3,
                                                     grepl("^FR", NUTS_ID),
                                                     !(NUTS_ID %in% c("FRY", "FRM"))) %>%
        dplyr::select(geometry) %>%
        sf::st_union()
      .
    } %>%
    # Add DEG0N to DEG0P
    {
      .$geometry[.$NUTS_ID == "DEG0P"] <- sf::st_union(.$geometry[.$NUTS_ID == "DEG0P"],
                                                       .$geometry[.$NUTS_ID == "DEG0N"])
      dplyr::filter(., NUTS_ID != "DEG0N")
    } %>%
    # Keep only those needed
    dplyr::filter(grepl("^DE|^DK$|^PL$|^CZ$|^AT$|^CH$|^FR$|^LU$|^BE$|^NL$", NUTS_ID)) %>%
    # Rename column
    dplyr::rename(region = NUTS_ID)

  # save German region geoms
  de_geoms <- areas %>%
    dplyr::filter(grepl("^DE", region)) %>%
    dplyr::arrange(region)

  # save neighbour country geoms
  neighbour_geoms <- areas %>%
    # keep only neighbouring countries
    dplyr::filter(region %in% c("DK", "PL", "CZ", "AT", "CH", "FR", "LU", "BE", "NL")) %>%
    dplyr::arrange(region)

  # create out object
  out_obj <- list(
    nuts_0 = de_geoms %>% dplyr::filter(nchar(region) == 2),
    nuts_1 = de_geoms %>% dplyr::filter(nchar(region) == 3),
    nuts_2 = de_geoms %>% dplyr::filter(nchar(region) == 4),
    nuts_3 = de_geoms %>% dplyr::filter(nchar(region) == 5),
    neighbours = neighbour_geoms)

  # save this to disk
  saveRDS(out_obj,
          file = make_path(cache_dir, filename))
  cat("Created processed geometries in", make_path(cache_dir, filename), fill = TRUE)

  # return 0
  return(out_obj)
}

#' Download and unzip shapefiles from Eurostat
#'
#' @template cache_dir
#' @return This function invisibly returns \code{0} if everything worked well
#'
#' @importFrom utils download.file unzip
get_shapefiles_from_source <- function(cache_dir){

  shapefile_dir <- "shapefiles"

  url <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-01m.shp.zip"
  utils::download.file(url, destfile = make_path(cache_dir, "nuts_shp.zip"), quiet = TRUE)
  files <- utils::unzip(make_path(cache_dir, "nuts_shp.zip"), exdir = make_path(cache_dir, shapefile_dir))
  unlink(make_path(cache_dir, "nuts_shp.zip"))
  zip_files <- files[grepl(".zip$", files)]
  shapefiles <- invisible(sapply(zip_files, function(x){
    dir_names <- gsub(".shp.zip$", "", x)
    if(!dir.exists(dir_names)) dir.create(dir_names)
    utils::unzip(x, exdir = dir_names)
    return(dir_names)
  }, USE.NAMES = FALSE))
  unlink(zip_files)
  cat("Created shapefiles in directory", make_path(cache_dir, shapefile_dir), fill = TRUE)
  return(invisible(0L))
}
