#' Get temperature data set from European Climate Assessment and Dataset
#' 
#' @description This function provides access to temperature data sourced from \insertCite{DEcovid:TEMPdata;textual}{DEcovid}. 
#' For more info on the output see the "Value" section.
#' 
#' @template time_res
#' @template spat_res
#' @template age_res
#' @param complete A \code{character} vector of length 1 which determines the level of processing. If \code{"none"}, returns
#' the time series at each station with potential missing data. If \code{"station"}, the potentially missing temperatures are
#' completed by averaging the temperatures at the three closest weather stations that have non-missing data available for the
#' same day. If \code{"region"}, the temperature time series at all stations within a NUTS-3 region (plus those within \code{tol}
#' kilometers of the region boundary) are averaged in order to return a single temperature time series per NUTS-3 region. If there
#' is no station within a NUTS-3 region plus the tolerance, the time series for that region is calculated as an average of the
#' region's neighbours.
#' @param tol The tolerance in kilometers from the NUTS-3 region border that is used to determine whether a weather station
#' is still considered as relevant for the temperature within a specific region. This argument is ignored if \code{complete = "none"} or \code{complete = "station"}.
#' @template country
#' @param start_date The start date of the period of interest formatted according to ISO 8601 given as a \code{character} vector of length 1.
#' @param end_date Either \code{NULL} (the default) or the end date of the period of interest formatted according to 
#' ISO 8601 given as a \code{character} vector of length 1. If \code{NULL}, the function returns data for the period starting at \code{start_date} until
#' the latest point in time for which data is available.
#' @template cache_dir
#' @template enforce_cache
#'
#' @return If \code{complete = "none"} or \code{complete = "station"} the function returns a \code{tibble} containing all weather stations within the selected
#' country with location and temperature time series. It contains the following columns:
#' \describe{
#'   \item{STAID}{The ID of the weather station.}
#'   \item{STANAME}{The name of the weather station. This usually indicates a city or town.}
#'   \item{CN}{The ISO 3166-1 alpha-2 code of the country, the station is located in.}
#'   \item{HGHT}{The elevation of the weather station in meters above sea level.}
#'   \item{ts}{A list column containing \code{tibble}s with the temperature time series at each station. Each of the temperature measurements
#'   also has a binary quality indicator where 1 denotes that the measurement is suspect and 0 means that the measurement is fine. If \code{complete = "none"} 
#'   the raw data as published by \insertCite{DEcovid:TEMPdata;textual}{DEcovid} is returned which means that the temperature time series may contain missing values
#'   on some days. If \code{complete = "station"} the missing values are imputed averaging the temperatures measured at the closest three weather stations that 
#'   have non-missing values on the specific day.}
#'   \item{geometry}{The spatial location of the weather stations in the EPSG 3035 coordinate system.}
#'   \item{n_suspect}{The number of suspect measurements within the time series of each weather station.}
#'   \item{n_na}{The number of missing measurements within the time series of each weather station.}
#' } \cr
#' If \code{complete = "region"} the temperature time series are aggregated to the NUTS 3 level by averaging the time series of all weather stations within
#' the specific NUTS region or within \code{tol} kilometers of it's boundaries. The result in this case is a nested \code{tibble} containing columns \emph{region} and
#' \emph{ts} where \emph{region} specifies the NUTS-3 region and \emph{ts} contains \code{tibble}s with the temperature time series for each NUTS-3 region. \cr
#' If \code{time_res}, \code{spat_res}, and \code{age_res} are not \code{NULL}, the function returns a \code{tibble} with 
#' columns \emph{region}, \emph{date}, \emph{age}, and \emph{value}. The aggregation over space to larger regions is done by averaging the temperature measurements
#' of all NUTS-3 regions within the larger region. Aggregation over time to weekly resolution is also done by averaging the temperatures within the same calendar week.
#' Subsequently, the data is simply expanded to fit the chosen age group resolution as temperatures are constant across age groups. 
#' 
#' 
#' @export
#' 
#' @references 
#' \insertRef{DEcovid:TEMPdata}{DEcovid} \cr\cr
#' \insertRef{DEcovid:TEMPsource}{DEcovid}
#'
#' @importFrom units set_units
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @importFrom tidyr unnest expand_grid
#'
#' @examples
#' \dontrun{
#' temperature <- get_temperature(time_res = "weekly",
#'                                spat_res = 1L,
#'                                age_res = "no_age",
#'                                country = "DE",
#'                                start_date = "2020-01-01",
#'                                tol = 2,
#'                                complete = "region")
#' }
get_temperature <- function(time_res = NULL,
                            spat_res = NULL,
                            age_res = NULL,
                            complete = c("none", "station", "region"),
                            tol = 0,
                            country = "DE",
                            start_date = "2020-01-01",
                            end_date = NULL,
                            cache_dir = NULL,
                            enforce_cache = FALSE){

  # Check inputs
  join <- check_res_args(time_res = time_res,
                         spat_res = spat_res,
                         age_res = age_res)
  check_enforce_cache(enforce_cache = enforce_cache)
  
  if(!is.null(end_date) && as.Date(start_date) > as.Date(end_date))
    stop("End date must be later than start date.")
  
  # nb_pattern used in this function
  nb_pattern <- "F***1****" # Use "F***T****" if a point where geoms touch is enough for two regions to be neighbours
  complete <-  match.arg(complete)
  if(length(complete) != 1L){
    stop("The 'complete' argument must be of length 1.")
  }
  if(!is.numeric(tol) || tol < 0){
    stop("The 'tol' argument must be numeric and larger than 0.")
  }
  if(join){
    if(complete != "region")
      warning("Ignoring argument \"complete = '", complete, "'\" and setting it to 'region' as otherwise aggregation does not work.")
    complete <- "region"
  }
  if(complete == "region"){
    tol <- units::set_units(tol, "km")
  }


  # set parameters for cacheing
  filename <- "temperature.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 15, units = "days")

  # get pre-processed data from file or from source
  if(enforce_cache){
    if(!file.exists(make_path(cache_dir, filename))){
      stop("There is no cached version of the requested data in 'cache_dir' directory.")
    } else {
      temperature <- readRDS(make_path(cache_dir, filename))
    }
  } else {
    if(from_cache){
      temperature <- readRDS(make_path(cache_dir, filename))
    } else {
      temperature <- process_temperature(country = country, start_date = start_date, end_date = end_date, 
                                         cache_dir = cache_dir, filename = filename)
    }
  }

  # if completed stations required, complete
  if(complete %in% c("station", "region")){
    temperature <- complete_temp_ts(temperature)
  }

  # if per NUTS-3 region required, summarise
  if(complete == "region"){
    temperature <- summarise_temp_to_region(temperature = temperature, tol = tol,
                                            nb_pattern = nb_pattern, cache_dir = cache_dir)
  }

  # aggregate if desired
  if(join){
    temperature <- tidyr::unnest(temperature, ts)
    dims <- get_case_info(spat_res = 3, time_res = "daily", cache_dir = cache_dir)
    region <- dims$region
    age <- dims$age
    date <- dims$date
    # print(tidyr::expand_grid(age = age, date = date, region = region))
    # print(temperature)
    grid <- tidyr::expand_grid(age = age, date = date, region = region)
    temperature <-  grid %>%
      dplyr::left_join(y = temperature, by = c("date", "region")) %>%
                         summarise_data(time_res = time_res, spat_res = spat_res, age_res = age_res,
                                        time_f = time_f_temperature, spat_f = spat_f_temperature, age_f = age_f_temperature)
  }

  return(temperature)
}

#' Summarise the completed time series of all stations to NUTS-3 region specific temperature time series.
#'
#' @param temperature The output \code{tibble} of \code{complete_temp_ts()}.
#' @param tol The tolerance in kilometers from the NUTS-3 region border that is used to determine whether a weather station
#' is still considered as relevant for the temperature within a specific region.
#' @param nb_pattern The neighbourhood pattern that is used to calculate the neighbourhood structure.
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{region} and \code{ts}. The column \code{ts} is a nested column and
#' contains the temperature time series for the respective NUTS-3 region.
#'
#' @importFrom sf st_distance st_drop_geometry st_relate st_crs st_transform
#' @importFrom dplyr tibble group_by summarise mutate rename bind_rows select arrange
#' @importFrom tidyr unnest nest
summarise_temp_to_region <- function(temperature, tol, nb_pattern, cache_dir){

  # get geometries of NUTS-3 regions
  geoms <- get_geoms(cache_dir = cache_dir)[[4]]
  geoms <- sf::st_transform(geoms, sf::st_crs(temperature))

  # calculate distances from weather stations to region polygons
  dist <- sf::st_distance(temperature$geometry, geoms$geometry)

  # for every region get all stations within the the defined distance from the polygon
  # and calculate mean temperature
  temp_list <- lapply(seq_len(nrow(geoms)), function(x){
    idx <- order(dist[, x], decreasing = FALSE)
    tb <- dplyr::tibble(weather_station = idx,
                        distance = dist[idx, x])
    if(tb$distance[1] <= tol){
      tb <- tb[which(tb$distance <= tol), ]
      temperature[tb$weather_station, c("STANAME", "ts")] %>%
        sf::st_drop_geometry() %>%
        tidyr::unnest(cols = "ts") %>%
        # average temperatures for each day
        dplyr::group_by(date) %>%
        dplyr::summarise(temperature = mean(temperature)) %>%
        # add region and rename
        dplyr::mutate(region = geoms$region[x]) %>%
        dplyr::rename(value = temperature)
    } else {
      return(NA)
    }
  })
  names(temp_list) <- geoms$region

  # if there are regions that have no station within the tolerance
  # compute spatial neighbourhood and take mean of neighbours
  if(any(is.na(temp_list))){
    # calculate neighbourhood order
    adj_order <- sf::st_relate(geoms, pattern = nb_pattern, sparse = TRUE)
    # fill empty list components
    no_temp_regions <- which(is.na(temp_list))
    temp_list[no_temp_regions] <- lapply(no_temp_regions, function(i){
      # get temperatures of neighbours
      nb_temp_list <- temp_list[adj_order[[i]]]
      # throw out neighbours without weather stations
      nb_temp_list <- nb_temp_list[!is.na(nb_temp_list)]
      # Throw errors if none of the neighbours have temperature data
      if(length(nb_temp_list) == 0L) stop("Region", names(temp_list)[i], "does not have any neighbours with temperature data.\n")
      # Calculate average
      nb_temp_list %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(value = mean(value), .groups = "drop") %>%
        dplyr::mutate(region = names(temp_list)[i]) %>%
        dplyr::select(date, value, region) %>%
        dplyr::arrange(date)
    })
  }

  # return object
  temp_list <- temp_list %>%
    dplyr::bind_rows() %>%
    tidyr::nest(ts = c("date", "value"))

  return(temp_list)
}


#' Estimate the temperature on a given day for a given station
#'
#' @param temperature The output \code{tibble} of \code{process_temperature()}.
#'
#' @return The same \code{tibble} as the input but with completed temperature time series.
#' @details The temperature time series are completed through the following approach:
#' \enumerate{
#'   \item{Find a day with missing temperature in a time series of a given station.}
#'   \item{Get the three closest stations that have non-missing temperature on the respective day.}
#'   \item{Average the temperature of these three closest stations for the given day.}
#' }
#'
#' @importFrom sf st_distance
#' @importFrom purrr map
complete_temp_ts <- function(temperature){

  # calculate distances between weather stations
  dist_ws <- sf::st_distance(temperature$geometry)

  # complete the temperature sets (if there is an NA for a certain station on a certain day,
  # calculate the average of the three closest stations that are non NA)
  temperature$ts <- purrr::map(seq_len(nrow(temperature)), function(x){
    if(temperature$n_na[x] == 0){
      return(temperature$ts[[x]])
    } else {
      # which days are NA
      idx <- which(is.na(temperature$ts[[x]]$temperature))
      # construct output object
      out <- temperature$ts[[x]]
      closest_stations <- order(dist_ws[, x])[-1]
      # Check 3 closest stations with actual temperatures and average temperature for that day
      out$temperature[idx] <- vapply(idx, function(y){
        closest_station_temperatures <- rep(NA_real_, 3L)
        found <- 0
        counter <- 1
        while(any(is.na(closest_station_temperatures))){
          value <- temperature$ts[[closest_stations[counter]]]$temperature[y]
          if(!is.na(value)){
            closest_station_temperatures[found + 1] <- value
            found <- found + 1
          }
          counter <- counter + 1
        }
        return(mean(closest_station_temperatures))
      }, double(1L))
      return(out)
    }
  })

  return(temperature)
}

#' Import and process the data from the ECAD textfiles.
#'
#' @param country An ISO 3166-1 abbreviation for the country of interest.
#' @param start_date The start date of the period of interest formatted according to ISO 8601 given as a \code{character} vector of length 1.
#' @param end_date Either \code{NULL} (the default) or the end date of the period of interest formatted according to 
#' ISO 8601 given as a \code{character} vector of length 1. If \code{NULL}, the function returns data for the period starting at \code{start_date} until
#' the latest point for which data is available.
#' @template cache_dir
#' @param filename The filename under which the processed temperature data should be stored in \code{cache_dir}.
#'
#' @return The function returns a \code{tibble} containing weather stations in Germany that provide non-missing data
#' between the 1st of January and the end of the data set. All temperature time series are of the same length.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter %>% mutate select if_else left_join
#' @importFrom parallel mclapply detectCores
#' @importFrom sf st_as_sf st_set_crs st_transform
#' @importFrom purrr map_dbl map_int
#'
process_temperature <- function(country, start_date, end_date, cache_dir, filename){

  # What the directory with the text files is called
  txt_dir <- "ECA_blend_tg"

  # Find out whether to use cached textfiles
  cache_dir <- get_cache_dir(cache_dir)
  dir_path <- make_path(cache_dir, txt_dir)
  make_new <- TRUE
  if(dir.exists(dir_path)){
    make_new <- needs_update(dir_path, 60, "days")
  }

  # Download temperature textfiles if necessary
  if(make_new){
    get_temperature_archive_from_source(cache_dir)
  }

  # Get all the files
  weather_files <- list.files(make_path(cache_dir, "ECA_blend_tg")) %>%
    subset(grepl("STAID", .)) %>%
    vapply(function(x){make_path(make_path(cache_dir, "ECA_blend_tg"), x)}, character(1L), USE.NAMES = FALSE)

  # import station overview and convert LAT/LON to decimal
  temperature <- readLines(make_path(cache_dir, make_path("ECA_blend_tg", "stations.txt")))
  idx <- unique(c(1:17, which(temperature == "")))
  temperature <- paste(temperature[-idx], collapse = " \n")
  temperature <- readr::read_csv(temperature,
                                 show_col_types = FALSE, progress = FALSE,
                                 trim_ws = TRUE) %>%
    dplyr::filter(CN %in% country) %>%
    {
      if(nrow(.) == 0L) stop(paste0("No stations found for country ", country, "."))
      .
    } %>% 
    dplyr::mutate(LAT = vapply(strsplit(gsub("\\+", "", LAT), ":"), function(x){x <- as.numeric(x); x[1] + x[2]/60 + x[3]/3600}, double(1L)),
                  LON = vapply(strsplit(gsub("\\+", "", LON), ":"), function(x){x <- as.numeric(x); x[1] + x[2]/60 + x[3]/3600}, double(1L)))
  
  # Compute what dates the time series should have
  earliest <- as.Date(start_date) # when should time series start
  latest <- if(is.null(end_date)) # when should time series end
    Sys.Date() else as.Date(end_date)
  desired_dates <- seq(earliest, latest, 1L)
  
  # Add temperature time series
  temperature$ts <- parallel::mclapply(weather_files, function(x){
    #system(paste0("echo ", x))
    # Throw out stuff not needed
    ts <- gsub("\\s", "", readLines(x))
    ts <- ts[ts != ""]
    ts <- ts[grep("STAID.*SOUID.*DATE.*TG.*Q_TG", ts):length(ts)]
    
    # Find out which lines to actually read
    ts_start <- as.Date(sub("^\\d+,\\d+,(\\d+),.+$", "\\1", ts[2]), format = "%Y%m%d")
    ts_end <- as.Date(sub("^\\d+,\\d+,(\\d+),.+$", "\\1", ts[length(ts)]), format = "%Y%m%d")
    idx <- seq(ts_start, ts_end, 1L) %in% desired_dates
    if(!any(idx)){
      return(dplyr::tibble(DATE = desired_dates,
                           TG = NA_real_,
                           Q_TG = 0L))
    }
    ts <- ts[c(TRUE, idx)]
    
    ts <- ts %>%
      paste(collapse = " \n") %>%
      readr::read_csv(col_types = "iicni",
                      trim_ws = TRUE) %>%
      dplyr::select(DATE, TG, Q_TG) %>%
      dplyr::mutate(TG = dplyr::if_else(Q_TG == 9L, NA_real_, TG/10), # replace missing temperature with NAs and convert to degrees
                    Q_TG = dplyr::if_else(Q_TG == 9L, 0L, Q_TG), # set quality indicator for missing temperature to 0 (as we just introduced NAs)
                    DATE = as.Date(DATE, format = "%Y%m%d")) %>%  # convert date to date
      dplyr::left_join(dplyr::tibble(DATE = desired_dates), ., by = "DATE") # complete the time series
    return(ts)
  }, mc.cores = parallel::detectCores() - 1)
  
  # Remove unnecessary NAs in the end
  idx <- vapply(temperature$ts, function(x){
    non_na <- !is.na(x$TG)
    c(which(non_na)[1], length(non_na) - which(rev(non_na))[1] + 1L)
  }, integer(2L))
  idx <- c(min(idx[1, ], na.rm = TRUE), max(idx[2, ], na.rm = TRUE))
  temperature <- temperature %>% 
    dplyr::mutate(ts = lapply(ts, function(x) x[seq(idx[1], idx[2], 1L), ]) )
  
  # Set CRS and filter out stations with NA only
  temperature <- sf::st_as_sf(temperature, coords = c("LON", "LAT")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(3035) %>%
    # do some quality control
    ## check quality codes(original codes: 0 = valid, 1 = suspect, 9 = missing)
    ## here we have set codes to 0 = valid, 1 = suspect
    ## if Q_TG was originally set to 9, we replaced temperature with NA and set the status code to 0
    dplyr::mutate(n_suspect = purrr::map_dbl(ts, function(x){sum(x$Q_TG, na.rm = TRUE)}),
                  n_na = purrr::map_dbl(ts, function(x){sum(is.na(x$TG))}),
                  ts = purrr::map(ts, function(x){
                    x %>%
                      dplyr::select(DATE, TG, Q_TG) %>%
                      dplyr::rename(date = DATE, temperature = TG, quality = Q_TG)
                  })
                  ) %>%
    dplyr::filter(purrr::map_int(ts, function(x){nrow(x)}) != n_na)

  # save this
  saveRDS(temperature, file = make_path(cache_dir, filename))

  # remove the 1 GB EC_blend_tg directory
  unlink(dir_path, recursive = TRUE)

  # return temperature object
  return(temperature)
}

#' Download and unzip temperature textfiles from ECA
#'
#' @template cache_dir
#' @return This function invisibly returns \code{0} if everything worked well
#'
#' @importFrom utils download.file unzip
get_temperature_archive_from_source <- function(cache_dir){

  out_dir <- "ECA_blend_tg"

  # get data
  dir.create(make_path(cache_dir, out_dir), showWarnings = FALSE)
  "https://knmi-ecad-assets-prd.s3.amazonaws.com/download/ECA_blend_tg.zip" %>%
    utils::download.file(destfile = make_path(cache_dir, "ECA_blend_tg.zip"), quiet = TRUE)

  # unzip file that shows which stations are where
  dir <- make_path(cache_dir, out_dir)
  file <- "stations.txt"
  filepath <- make_path(dir, file)
  utils::unzip(make_path(cache_dir, "ECA_blend_tg.zip"), files = file, exdir = dir)

  # import station overview
  temperature <- readLines(filepath)
  idx <- unique(c(1:17, which(temperature == "")))
  temperature <- paste(temperature[-idx], collapse = " \n")
  temperature <- readr::read_csv(temperature,
                                 show_col_types = FALSE, progress = FALSE,
                                 trim_ws = TRUE)

  # check which stations are in Germany and get their ID
  temperature <- temperature %>%
    dplyr::filter(CN == "DE")

  # unzip only the files from Germany
  id <- vapply(temperature$STAID, function(x){
    paste0(paste0(rep(0, 6 - nchar(x)), collapse = ""), x)
  }, character(1L))
  unzip_files <- paste0("TG_STAID", id, ".txt")
  utils::unzip(make_path(cache_dir, "ECA_blend_tg.zip"), files = unzip_files, exdir = dir) # this takes quite a while

  # remove zip archive
  unlink(file.path(cache_dir, "ECA_blend_tg.zip"))

  return(invisible(0L))
}
