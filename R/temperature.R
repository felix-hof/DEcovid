#' Get temperature data set for this thesis
#'
#' @param completed A \code{logical} of length 1 which indicates whether or not to estimate missing values from
#' neighbouring weather stations.
#' @template cache_dir
#'
#' @return A \code{tibble} containing all weather stations within Germany
#' with location and temperature time series starting on 1 January 2020.
#' @export
#'
#' @examples
#' \dontrun{
#' temperature <- get_temperature()
#' }
get_temperature <- function(completed = FALSE, cache_dir = NULL){

  # set parameters for cacheing
  filename <- "temperature.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 15, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    temperature <- readRDS(make_path(cache_dir, filename))
  } else {
    if(file.exists(make_path(cache_dir, "ECA_blend_tg"))) unlink(make_path(cache_dir, "ECA_blend_tg"))
    temperature <- process_temperature(cache_dir, filename)
  }

  if(completed){
    temperature <- complete_temp_ts(temperature)
  }

  return(temperature)
}

#' Estimate the temperature on a given day for a given station
#'
#' @param temperature The output \code{tibble} of \code{process_temperature}.
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
        closest_station_temperatures <- rep(NA_real_, 3)
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
#' @template cache_dir
#' @param filename The filename under which the processed temperature data should be stored in \code{cache_dir}.
#'
#' @return The function returns a \code{tibble} containing weather stations in Germany that provide non-missing data
#' between the 1st of January and the end of the data set. All temperature time series are of the same length.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter %>% mutate select if_else
#' @importFrom parallel mclapply detectCores
#' @importFrom sf st_as_sf st_set_crs st_transform
#' @importFrom purrr map_dbl map_int
#'
process_temperature <- function(cache_dir, filename){

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
    dplyr::filter(CN == "DE") %>%
    dplyr::mutate(LAT = vapply(strsplit(gsub("\\+", "", LAT), ":"), function(x){x <- as.numeric(x); x[1] + x[2]/60 + x[3]/3600}, double(1L)),
                  LON = vapply(strsplit(gsub("\\+", "", LON), ":"), function(x){x <- as.numeric(x); x[1] + x[2]/60 + x[3]/3600}, double(1L)))

  # Add temperature time series
  temperature$ts <- parallel::mclapply(weather_files, function(x){

    # Throw out stuff not needed
    ts <- gsub("\\s", "", readLines(x))
    ts <- ts[ts != ""]
    ts <- ts[grep("STAID.*SOUID.*DATE.*TG.*Q_TG", ts):length(ts)]

    # Find out which lines to actually read
    earliest <- as.Date("2020-01-01") # when to start each time series
    ts_start <- as.Date(gsub("^\\d+,\\d+,(\\d+),.+$", "\\1", ts[2]), format = "%Y%m%d")
    if(ts_start <= earliest){
      idx <- grep(as.character(format.Date(earliest, format = "%Y%m%d")), ts)
      ts <- ts[c(1, idx:length(ts))]
    } else { # complete the time series
      STAID <- sub("^(\\d+),.+$", "\\1", ts[2])
      SOUID <- sub("^\\d+,(\\d+),.+$", "\\1", ts[2])
      ENDSEQ <- as.Date(sub("^\\d+,\\d+,(\\d+),.+$", "\\1", ts[2]), format = "%Y%m%d") - 1
      DATE <- format(seq(earliest, ENDSEQ, by = 1), "%Y%m%d")
      TG <- 9999
      Q_TG <- 9
      ts <- c(ts[1],
              paste0(STAID, ",", SOUID, ",", DATE, ",", TG, ",", Q_TG),
              ts[2:length(ts)])
    }

    ts <- ts %>%
      paste(collapse = " \n") %>%
      readr::read_csv(col_types = "--cnc",
                      trim_ws = TRUE) %>%
      dplyr::select(DATE, TG, Q_TG) %>%
      dplyr::mutate(TG = dplyr::if_else(Q_TG == 9, NA_real_, TG/10), # replace missing temperature with NAs and convert to degrees
                    Q_TG = dplyr::if_else(Q_TG == 9, "0", Q_TG), # set quality indicator for missing temperature to 0 (as we just introduced NAs)
                    DATE = as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\3", DATE))) # convert date to date
    return(ts)
  }, mc.cores = parallel::detectCores() - 1)

  # Set CRS and filter out stations with NA only
  temperature <- sf::st_as_sf(temperature, coords = c("LON", "LAT")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(3035) %>%
    # do some quality control
    ## check quality codes(original codes: 0 = valid, 1 = suspect, 9 = missing)
    ## here we have set codes to 0 = valid, 1 = suspect
    ## if Q_TG was originally set to 9, we replaced temperature with NA and set the status code to 0
    dplyr::mutate(n_suspect = purrr::map_dbl(ts, function(x){sum(as.numeric(x$Q_TG))}),
                  n_na = purrr::map_dbl(ts, function(x){sum(is.na(x$TG))}),
                  ts = purrr::map(ts, function(x){
                    x %>%
                      dplyr::select(DATE, TG) %>%
                      dplyr::rename(date = DATE, temperature = TG)
                  })) %>%
    dplyr::filter(purrr::map_int(ts, function(x){nrow(x)}) != n_na)

  # save this
  saveRDS(temperature, file = make_path(cache_dir, filename))

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
