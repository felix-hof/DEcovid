#' @importFrom Rdpack reprompt
NULL

#' Get cache directory
#' @description Handles cache directory for data files used within the \code{suspendr} package.
#' @template cache_dir
#' @return The path to the cache directory. Object of class \code{character}.
#'
get_cache_dir <- function(cache_dir){
  if(!is.null(cache_dir)){
    return(cache_dir)
  } else if(!is.null(getOption("DEcovid_cache_dir"))){
    return(getOption("DEcovid_cache_dir"))
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

#' Get the dates, age groups and regions present in the case data set
#'
#' @param spat_res The NUTS level on which to return the regions present in the case data set.
#' @param time_res The time resolution on which to return the dates present in the case data set.
#' @template cache_dir
#'
#' @return A \code{list} containing regions, age groups, and dates present in the case data set.
#'
get_case_info <- function(spat_res, time_res, cache_dir){
  if(all(file.exists(make_path(cache_dir,
                               c("agegroups.rds",
                                 paste0("case_regions_", spat_res, ".rds"),
                                 paste0("case_dates_", time_res, ".rds"))
                               )))){
    agegroups <- readRDS(make_path(cache_dir, "agegroups.rds"))
    regions <- readRDS(make_path(cache_dir, paste0("case_regions_", spat_res, ".rds")))
    dates <- readRDS(make_path(cache_dir, paste0("case_dates_", time_res, ".rds")))
  } else {
    get_cases(cache_dir = cache_dir)
    agegroups <- readRDS(make_path(cache_dir, "agegroups.rds"))
    regions <- readRDS(make_path(cache_dir, paste0("case_regions_", spat_res, ".rds")))
    dates <- readRDS(make_path(cache_dir, paste0("case_dates_", time_res, ".rds")))
  }
  return(list(date = dates, age = agegroups, region = regions))
}

# check resolution arguments
check_res_args <- function(spat_res, age_res, time_res){

  argsisnull <- vapply(list(spat_res, age_res, time_res), is.null, logical(1L))
  arglengths <- vapply(list(spat_res, age_res, time_res), length, integer(1L))

  if(!all(argsisnull + arglengths == 1L)){
    stop("Each of the arguments 'spat_res', 'age_res', and 'time_res' must be either NULL or length 1.")
  }

  if(!(any(c(sum(argsisnull), sum(arglengths)) %in% c(0L, 3L)))){
    stop("Arguments 'time_res', 'spat_res', and 'age_res' must be either all NULL or all non-NULL.")
  }

  if(!all(argsisnull)){
    if(!(spat_res %in% 0L:3L)){
      stop("Invalid argument 'spat_res'. The argument 'spat_res' must be one of c(0, 1, 2, 3).")
    }

    if(!(age_res %in% c("age", "no_age"))){
      stop("Invalid argument 'age_res'. The argument 'age_res' must be one of c(\"age\", \"no_age\").")
    }

    if(!(time_res %in% c("daily", "weekly"))){
      stop("Invalid argument 'time_res'. The argument 'time_res' must be one of c(\"daily\", \"weekly\").")
    }
  }

  return(c("join" = !all(argsisnull)))
}

# check enforce_cache argument
check_enforce_cache <- function(enforce_cache){
  if(length(enforce_cache) != 1L || !(enforce_cache %in% c(TRUE, FALSE))){
    stop("Argument 'enforce_cache' must be either TRUE or FALSE.")
  }
  return(invisible(NULL))
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
  "CN", "LAT", "LON", "ts", "DATE", "TG", "n_na", "Q_TG",
  # stringency.R
  "CountryCode", "Date", "StringencyIndex", "value", "join",
  # urbanicity.R
  "Bevoelkerung", "Land", "RB", "Kreis", "Gemeindename", "population", "value",
  # vaccination.R
  "Anzahl", "Impfdatum", "Impfschutz", "LandkreisId_Impfort", "age_case_end", "age_case_start",
  "age_group_case", "age_group_vac", "age_vac_end", "age_vac_start", "case_contained", "region1",
  "value.prop_unvac", "value.vac",
  # holidays.R
  "lvl1_name", "holiday", "join", "value.hol", "holidays",
  # testing.R
  "week", "ntests", "trate", "join",
  # area_size.R
  "lvl3_name", "landuse", "join",
  # aggregation_functions.R
  "region",
  # fit_models.R
  "name", "stratum_1", "stratum_2",
  # variants.R
  "country_code", "year_week", "valid_denominator", "number_sequenced", "number_detections_variant",
  "variant", "percent_cases",
  # density.R
  "value.pop", "value.area",
  # plotting.R
  "component",
  # table.R
  "new_name", "trans", "comp", "par", "latex", "est", "sderr",
  # weekdays.R
  "reference",
  # get_data.R
  "overview",
  # neighbour_matrices.R
  "age_lab"
))

