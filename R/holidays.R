#' Get holiday data from appfield.net
#'
#' @description Holiday data is scraped from \insertCite{DEcovid:holidays2020}{DEcovid}, \insertCite{DEcovid:holidays2021}{DEcovid}, and \insertCite{DEcovid:holidays2022}{DEcovid}.
#' It is available on Bundesland (NUTS 1) resolution. The data is processed such that binary indicators are created for each NUTS 1 region where 1 denots a day being a holiday.
#' Subsequently, the data is either expanded to fit lower smaller resolution (NUTS 2 or NUTS 1). Aggregation to the NUTS 0 level is done by calculating a population weighted average.
#' The weights are retrieved through \code{\link[DEcovid]{get_population}}. Therefore, on the NUTS 0 level, the result is not a binary indicator anymore but a value between 0 and 1.
#' Aggregation over time is implemented by summing up the indicators across the different strata. The result of weekly aggregated values is thus not a binary indicator anymore.
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#' @template enforce_cache
#'
#' @return If \code{time_res}, \code{spat_res} and \code{age_res} are all \code{NULL}, the function returns a \code{tibble} with 
#' columns \code{date}, \code{region} and \code{value} (contains the binary holiday indicator). Otherwise,
#' the function expands the data to also include a column \code{age} which contains age groups and summarises it to the desired
#' dimension resolutions.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @importFrom tidyr expand_grid
#' @importFrom utils data
#' 
#' @references 
#' \insertRef{DEcovid:holidays2020}{DEcovid}
#' \insertRef{DEcovid:holidays2021}{DEcovid}
#' \insertRef{DEcovid:holidays2022}{DEcovid}
#'
#' @export
#'
#' @examples
#' holidays <- get_holidays()
#' holidays <- get_holidays(time_res = "weekly", spat_res = 0L, age_res = "no_age")
#'
get_holidays <- function(time_res = NULL,
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
  filename <- "holidays.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 30, units = "days")

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
      #dat <- get_holidays_from_source(cache_dir, filename)
      utils::data("holidays", package = "DEcovid")
      dat <- holidays
    }
  }

  # aggregate if desired
  if(join){
    dims <- get_case_info(spat_res = 3, time_res = "daily", cache_dir = cache_dir)
    dat <- tidyr::expand_grid(age = dims$age, date = dims$date, region = dims$region) %>%
      dplyr::left_join(y = dat, by = c("date", "region"))
    if(spat_res == 0L){
      dat <- dat %>% 
        dplyr::left_join(get_population(spat_res = 3, time_res = "daily", age_res = "age", cache_dir = cache_dir),
                         by = c("age", "date", "region"),
                         suffix = c(".hol", ".pop")) %>% 
        dplyr::mutate(region = substr(region, 1L, 2L)) %>% 
        dplyr::group_by(age, date, region) %>% 
        dplyr::summarise(value = stats::weighted.mean(value.hol, w = value.pop), .groups = "drop") %>% 
        summarise_data(spat_res = 3L, time_res = time_res, age_res = age_res, # spat_res = 3L does no aggregation over space
                       time_f = time_f_holidays, spat_f = spat_f_holidays, age_f = age_f_holidays)
    } else {
      dat <- dat %>% 
        summarise_data(time_res = time_res, spat_res = spat_res, age_res = age_res,
                       time_f = time_f_holidays, spat_f = spat_f_holidays, age_f = age_f_holidays) 
    }
  }

  return(dat)
}

# Get holiday data from appfield.net
#
# @template cache_dir
# @param  filename The name of the file that the holiday data is stored in.
#
# @return A \code{tibble} with columns \code{date}, \code{region} and \code{value} (contains the binary holiday indicator).
#
# @importFrom rvest read_html html_elements html_text html_children
# @importFrom magrittr extract
# @importFrom purrr map
# @importFrom tidyr expand_grid
# @importFrom dplyr tibble left_join select distinct bind_rows
# @importFrom stringr str_extract_all
# @noRd
# get_holidays_from_source <- function(cache_dir, filename){
# 
#   # other stuff needed
#   ref1 <- nuts_table() %>%
#     dplyr::select(lvl1, lvl1_name) %>%
#     dplyr::distinct()
#   ref3 <- nuts_table() %>%
#     dplyr::select(lvl3, lvl3_name)
#   all_dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = 1)
# 
#   # scrape pages
#   pages <- purrr::map(format(all_dates, "%Y") %>% unique(), function(x){
#     paste0("https://urlaubstage-planen.de/feiertage-in-deutschland-", x, ".htm") %>%
#       rvest::read_html()
#   })
# 
#   # extract national holiday dates for all years
#   hols <- purrr::map(pages, function(x){
#     nat_hols <- x %>%
#       rvest::html_elements(".with-values") %>%
#       rvest::html_text() %>%
#       strsplit(" - ") %>%
#       lapply(function(x){
#         x[1]
#       }) %>%
#       unlist(recursive = TRUE) %>%
#       as.Date(format = "%d.%m.%Y")
#     bl_hols <- x %>%
#       rvest::html_elements(".content-frame-filled") %>%
#       {
#         idx <- .[] %>%
#           rvest::html_text() %>%
#           grep("(?=.*\\bgesetzlich.*\\b)(?=.*\\bFeiertag.*\\b)(?=.*\\bBundesl.?nd.*\\b)(?=.*\\bBundesl.?nd.*\\b)(?=.*\\d{2}\\.\\d{2}\\.\\d{4})",
#                                             perl = TRUE, .)
#         magrittr::extract(., idx)
#       } %>%
#       rvest::html_children() %>%
#       rvest::html_text() %>%
#       magrittr::extract(2:length(.)) %>%
#       magrittr::extract(!(seq_along(.) %in% seq(2, length(.), 3))) %>%
#       {
#         lapply(seq(2, 32, by = 2), function(y){
#           bl <- ref1$lvl1[which(ref1$lvl1_name == .[y - 1])]
#           stringr::str_extract_all(.[y], "\\d{2}\\.\\d{2}\\.\\d{4}", simplify = TRUE) %>%
#             as.vector() %>%
#             as.Date(format = "%d.%m.%Y") %>%
#             c(., nat_hols) %>%
#             sort(decreasing = FALSE) %>%
#             {
#               year <- format(.[1], format = "%Y")
#               dt <- all_dates[grep(year, as.character(all_dates))]
#               hol <- ifelse(dt %in% ., 1L, 0L)
#               dplyr::tibble(date = dt, region = bl, value = hol)
#             }
#         }) %>%
#           dplyr::bind_rows()
#        }
#   }) %>%
#     dplyr::bind_rows()
# 
#   # Expand this to lvl3 regions
#   dat <- tidyr::expand_grid(NUTS_3 = ref3$lvl3,
#                             date = all_dates) %>%
#     dplyr::mutate(NUTS_1 = substr(NUTS_3, 1, 3)) %>%
#     dplyr::left_join(hols, by = c("date", "NUTS_1" = "region")) %>%
#     dplyr::select(date, lvl3, value) %>%
#     dplyr::arrange(lvl3, date) %>%
#     dplyr::rename(region = lvl3)
# 
#   # save this
#   saveRDS(dat, file = make_path(cache_dir, filename))
#   cat(paste0("Created file '", make_path(cache_dir, filename), "'."))
# 
#   return(dat)
# }

#' A data set containing binary indicators for public holidays for each day and NUTS 3 region in Germany
#' 
#' @format A data frame with 451552 rows and 3 variables
#' 
"holidays"