#' Get population by age and NUTS-3 level
#'
#' @template cache_dir
#'
#' @return A \code{tibble} with columns \code{lvl3}, \code{age}, and \code{values}. The column \code{values} contains the population counts.
#' @export
#'
#' @examples
#' population <- get_population()
get_population <- function(cache_dir = NULL){

  # set parameters for cacheing
  filename <- "population.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 60, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_population_from_source(cache_dir, filename)
  }

  return(dat)
}

#' Get population data from Eurostat
#'
#' @template cache_dir
#' @param  filename The name of the file where population data is stored.
#'
#' @return A \code{tibble} with columns \code{age}, \code{lvl3} and \code{value} (contains population counts).
#'
#' @importFrom restatapi get_eurostat_data
#' @importFrom dplyr filter select mutate case_when group_by summarise
#'
get_population_from_source <- function(cache_dir, filename){

  # get data from eurostat
  dat <- restatapi::get_eurostat_data("demo_r_pjangrp3",
                                      stringsAsFactors = FALSE,
                                      cache = TRUE) %>%
    dplyr::filter(sex == "T", time == "2020",
                  grepl("^DE.{3}$", geo),
                  !(age %in% c("UNK", "TOTAL", "Y_GE90", "Y85-89"))) %>%
    dplyr::select(-unit, -sex, -time) %>%
    # Add DEG0N to DEG0P and accumulate by age group
    dplyr::mutate(geo = replace(geo, geo == "DEG0N", "DEG0P"),
                  age = dplyr::case_when(age == "Y_LT5" ~ "00-04",
                                         age == "Y5-9" ~ "05-14",
                                         age == "Y10-14" ~ "05-14",
                                         age == "Y15-19" ~ "15-34",
                                         age == "Y20-24" ~ "15-34",
                                         age == "Y25-29" ~ "15-34",
                                         age == "Y30-34" ~ "15-34",
                                         age == "Y35-39" ~ "35-59",
                                         age == "Y40-44" ~ "35-59",
                                         age == "Y45-49" ~ "35-59",
                                         age == "Y50-54" ~ "35-59",
                                         age == "Y55-59" ~ "35-59",
                                         age == "Y60-64" ~ "60-79",
                                         age == "Y65-69" ~ "60-79",
                                         age == "Y70-74" ~ "60-79",
                                         age == "Y75-79" ~ "60-79",
                                         age == "Y80-84" ~ "80+",
                                         age == "Y_GE85" ~ "80+",
                                         TRUE ~ age)) %>%
    {
      if(any(grepl("^Y", .$age))) stop("Something is wrong in the creation of population.rds. Check the age groups.")
      .
    } %>%
    # add up counts by region and age (add DEG0N to DEG0P)
    dplyr::group_by(geo, age) %>%
    dplyr::summarise(values = sum(values), .groups = "drop") %>%
    # Join by region and age (time independent)
    # dplyr::right_join(y = tidyr::expand_grid(date = .all_dates,
    #                                          age = unique(.$age),
    #                                          lvl3 = unique(.$geo)),
    #                   by = c("geo" = "lvl3", "age" = "age")) %>%
    dplyr::rename(lvl3 = geo, value = values) %>%
    {
      if(any(is.na(.$value))) stop("Error in construction of population.")
      .
    } %>%
    dplyr::arrange(lvl3, age)

  # save this
  saveRDS(dat, file = make_path(cache_dir, filename))

  return(dat)
}
