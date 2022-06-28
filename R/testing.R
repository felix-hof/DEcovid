#' Get testing data from \insertCite{DEcovid:TESTdata;textual}{DEcovid}
#' 
#' @description This function returns data on the testing rate which is defined as the number of tests that were 
#' registered by \insertCite{DEcovid:TESTdata;textual}{DEcovid} per million inhabitants per day in all of Germany. If 
#' the specific resolutions are specified, the testing rate is just extended across the regions and age groups.
#' This means that, given a particular day, all regions and age groups are assumed to have the same testing rate.
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{tibble} with columns \code{date} and \code{value} (contains the testing rate per day and 1'000 people).
#'
#' @importFrom tidyr expand_grid
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#'
#' @export
#' 
#' @references 
#' \insertRef{DEcovid:TESTdata}{DEcovid}
#'
#' @examples
#' testing <- get_testing()
#'
get_testing <- function(time_res = NULL,
                        spat_res = NULL,
                        age_res = NULL,
                        cache_dir = NULL,
                        enforce_cache = FALSE){

  # Check inputs
  join <- check_res_args(time_res = time_res,
                         spat_res = spat_res,
                         age_res = age_res)
  check_enforce_cache(enforce_cache = enforce_cache)

  # set parameters for cacheing
  filename <- "testing.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 7, units = "days")

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
      dat <- get_testing_from_source(cache_dir, filename)
    }
  }

  # aggregate if desired
  if(join){
    dims <- get_case_info(spat_res = 3, time_res = "daily", cache_dir = cache_dir)
    region <- dims$region
    age <- dims$age
    date <- dims$date
    dat <- tidyr::expand_grid(age = age, date = date, region = region) %>%
      dplyr::left_join(y = dat, by = "date") %>%
      summarise_data(time_res = time_res, spat_res = spat_res, age_res = age_res,
                     time_f = time_f_testing, spat_f = spat_f_testing, age_f = age_f_testing)
  }

  return(dat)
}

#' Get testing data from RKI
#'
#' @template cache_dir
#' @param  filename The name of the file that the testing data is stored in.
#'
#' @return A \code{tibble} with columns \code{date} and \code{value} (contains the "daily" testing rate).
#'
#' @importFrom utils download.file
#' @importFrom readxl read_xlsx
#' @importFrom magrittr set_colnames
#' @importFrom dplyr filter mutate row_number right_join tibble arrange rename
#' @importFrom ISOweek ISOweek2date
#' @importFrom tidyr fill
#'
get_testing_from_source <- function(cache_dir, filename){

  # get total population
  total_population <- sum(get_population(cache_dir = cache_dir)$value)

  # get testing data
  dat <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Testzahlen-gesamt.xlsx?__blob=publicationFile" %>%
    {
      utils::download.file(., destfile = make_path(cache_dir, "Testzahlen-gesamt.xlsx"), quiet = TRUE)
      readxl::read_xlsx(make_path(cache_dir, "Testzahlen-gesamt.xlsx"), sheet = "1_Testzahlerfassung")[, 1:3]
    } %>%
    # get this into format
    magrittr::set_colnames(c("week", "ntests", "npos")) %>%
    dplyr::filter(!grepl("^Summe", week)) %>%
    # assign earlier tests to week 10 (2020-03-02 to 2020-03-08)
    dplyr::mutate(week = replace(week, dplyr::row_number() == 1, "10/2020"),
                  week = gsub("", "", week)) %>%
    {
      .$week <- ISOweek::ISOweek2date(vapply(strsplit(.$week, "/"), function(x){
        if(nchar(x[1]) == 1) x[1] <- paste0("0", x[1])
        paste0(x[2], "-W", x[1], "-1")
      }, character(1L)))
      .
    } %>%
    # calculate testing rate per 100'000
    dplyr:: mutate(population = total_population,
                   trate = ntests * 1000 / population) %>%
    # make a full date sequence
    dplyr::right_join(y = dplyr::tibble(date = seq(min(.$week), max(.$week) + 6, by = 1)), by = c("week" = "date")) %>%
    dplyr::arrange(week) %>%
    dplyr::select(week, trate) %>%
    # divide by 7 to get "daily" testing rate
    dplyr::mutate(trate = trate / 7) %>%
    # prettify
    dplyr::rename(date = week, value = trate) %>%
    dplyr::arrange(date) %>%
    tidyr::fill("value", .direction = "down")

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
