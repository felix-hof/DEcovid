#' Get cache directory
#' @description Handles cache directory for data files used within the \code{suspendr} package.
#' @template cache_dir
#' @return The path to the cache directory. Object of class \code{character}.
#'
get_cache_dir <- function(cache_dir){
  if(!is.null(cache_dir)){
    return(cache_dir)
  } else if(!is.null(getOption("rthesis_cache_dir"))){
    return(getOption("rthesis_cache_dir"))
  } else {
    return(tempdir())
  }
}


#' Check whether a file is older than a defined cutoff
#' @description Returns \code{TRUE} if a file should be updated. Mainly to decide whether to
#' ignore cached versions of the file.
#' @param filepath Object of class \code{character}. Indicates the path to the file to check.
#' @param cutoff Object of class \code{numeric}. How many days/hours/minutes etc. we accept the
#' file to be old.
#' @param units Object of class \code{character}. Unit of \code{cutoff}. One of the possibilities
#' listed in \code{\link[base:difftime]{base::difftime()}}.
#' @return \code{TRUE} if the file is older than the cutoff and \code{FALSE} otherwise.
#' Object of class \code{logical}.
#'
needs_update <- function(filepath, cutoff, units){
  age <- as.numeric(difftime(Sys.time(), file.info(filepath)$mtime, units = units))
  if(age > cutoff){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Construct path to a file from input directory and a file name
#' @description This function constructs valid paths from a
#' number of character strings that may or may not end with a directory separator.
#' @param ... The names of the directories and/or files in the correct order.
#' @return Path to file for further use. Object of class \code{character}.
#' @importFrom purrr map
make_path <- function(...){
  args <- list(...)#lapply(as.list(substitute(...())), eval)
  n <- vapply(args, length, integer(1L))
  if(!(length(unique(n[n != 1L])) %in% c(0L, 1L))){
    stop("Invalid number of input lengths.")
  }
  maxn <- max(n)
  args <- lapply(args, function(x){
    out <- if(length(x) == 1) {rep(x, maxn)} else {x}
    gsub("\\/$", "", out)
  })
  vapply(seq_len(maxn), function(x){
    do.call("file.path", purrr::map(args, x))
  }, character(1L))
}

#' Read file from cache directory or get data from source
#' @description Returns a decision whether to get data from a cached file (\code{TRUE}) or
#' from source (\code{FALSE}). If \code{cache_dir} does not exist yet, the function also creates
#' the directory.
#' @template cache_dir
#' @param filename Object of class \code{character}. Name of the cached file.
#' @param cutoff Object of class \code{numeric}. How many days/hours/minutes etc. we accept the
#' file to be old.
#' @param units Object of class \code{character}. Unit of \code{cutoff}. One of the possibilities
#' listed in \code{\link[base:difftime]{difftime()}}.
#' @return Object of class \code{logical}. Returns \code{TRUE} if the cached file exists and is
#' not older than \code{cutoff} in the specified \code{units}. Otherwise returns \code{FALSE}.
#'
read_from_cache <- function(cache_dir, filename, cutoff, units){
  filepath <- make_path(cache_dir, filename)
  # check whether cache directory exists, if not create it and return false
  if(!dir.exists(cache_dir)){
    dir.create(cache_dir, recursive = TRUE)
    message("Created the following cache directory:\n", normalizePath(cache_dir))
    return(FALSE)
  }
  # if cached directory exists but the cache file does not, return false
  if(!file.exists(filepath)) return(FALSE)
  if(needs_update(filepath = filepath, cutoff = cutoff, units = units)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Get data from Eurostat via the API instead of the bulk download facility
#'
#' @param dataset A \code{character} vector of length 1. The ID of a Eurostat data set.
#' @param args A \code{list} object. Contains further arguments used to filter \code{dataset}
#' server side.
#'
#' @return A \code{tibble} with the requested data.
#' @details The argument \code{args} can be looked up using the
#' \href{https://ec.europa.eu/eurostat/web/json-and-unicode-web-services/getting-started/query-builder}{Eurostat query builder}.
#'
#' @importFrom httr GET content
#' @importFrom purrr map cross_df
#' @importFrom dplyr arrange across bind_cols tibble mutate
#' @importFrom tidyr everything
#'
#' @examples
#' dataset <- "reg_area3"
#' args <- list(geoLevel = paste0("nuts", 1:3),
#'              filterNonGeo = "1",
#'              precision = "1",
#'              unit = "KM2",
#'              landuse = "TOTAL",
#'              shortlabel = "1",
#'              time = "2016")
#' area_size <- eurostat_query(dataset = dataset, args = args)
#'
#' @export
#'
eurostat_query <- function(dataset, args){
  # build query
  base_url <- "http://ec.europa.eu/eurostat/wdds/rest/data/v2.1/json/en/"
  args <- sapply(seq_along(args), function(x){
    paste0(names(args)[x], "=",unlist(args[x], use.names = FALSE), collapse = "&")
  })
  query <- paste0(base_url, dataset, "?", paste0(args, collapse = "&"))
  # get response
  res <- httr::GET(query)
  # extract data if request contains status code 200
  if(res$status_code == 200){
    res <- httr::content(res)
    # https://ec.europa.eu/eurostat/data/database/information see here for a list of flags like ":" for missing data
    out <- purrr::map(res$dimension, function(x) names(x$category$index)) %>%
      purrr::cross_df() %>%
      dplyr::arrange(dplyr::across(tidyr::everything())) %>%
      {
        data1 <- unlist(res$value, use.names = TRUE)
        data2 <- unlist(res$status, use.names = TRUE)
        data2[grepl(":", data2)] <- NA_real_
        data2 <- data2[is.na(data2)] # ignore the other status flags
        dplyr::bind_cols(.,
                         dplyr::tibble(value = c(data1, data2)) %>%
                           dplyr::mutate(index = c(names(data1), names(data2)),
                                         index = as.numeric(index)) %>%
                           dplyr::arrange(index))
      } %>%
      dplyr::mutate(value = as.numeric(value))
    return(out)
  } else {
    stop(paste0("The following Eurostat query: \n",
                query, " \n",
                "did not exit with status code 200 (",
                res$status_code, ")."))
  }
}

# Handle global variables for R CMD check ----

utils::globalVariables(c(
  # utils.R
  "index",
  # get_cases.R
  ".", "Meldedatum", "Altersgruppe", "IdLandkreis", "AnzahlFall", "NeuerFall", "AnzahlTodesfall",
  "NeuerTodesfall", "NeuerTodesfall", "new_cases", "new_deaths", "adm_unit", "lvl3", "age", "region",
  # nuts_table.R
  "features", "LK", "lvl3", "adm_unit", "NUTS_ID", ".", "NAME_LATN", "lvl2", "lvl1",
  # geoms.R
  "geometry",
  # population.R
  "geo", "sex", "time", "unit", "values",
  # temperature.R
  "CN", "LAT", "LON", "ts", "DATE", "TG", "n_na",
  # stringency.R
  "CountryCode", "Date", "StringencyIndex", "value",
  # urbanicity.R
  "Bevoelkerung", "Land", "RB", "Kreis", "Gemeindename", "population", "value",
  # vaccination.R
  "personen_erst_kumulativ", "unvac", "log_unvac",
  # holidays.R
  "lvl1_name", "holiday",
  # testing.R
  "week", "ntests", "trate",
  # area_size.R
  "lvl3_name", "landuse",
  # aggregation_functions.R
  "region",
  # fit_models.R
  "name"
))

