#' Get the log-proportion of unvaccinated people by age and region
#'
#' @template time_res
#' @template spat_res
#' @template age_res
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{tibble} containing the log-proportion of unvaccinated people stratified by region, age and date.
#' The exact resolution depends on the arguments.
#' @export
#' @importFrom tidyr expand_grid
#' @importFrom dplyr mutate left_join group_by summarise filter
#' @importFrom stats weighted.mean
#' 
#' @references 
#' \insertRef{DEcovid:VACdata}{DEcovid} \cr\cr
#' \insertRef{DEcovid:VACdatadef}{DEcovid}
#' 
#' @details The data is retrieved from \insertCite{DEcovid:VACdata;textual}{DEcovid} and additional information can be found in \insertCite{DEcovid:VACdatadef;textual}{DEcovid}.
#'
#' @examples
#' vac <- get_vaccination()
#' vac <- get_vaccination(time_res = "daily", spat_res = 3L, age_res = "no_age")
#'
get_vaccination <- function(time_res = NULL,
                            spat_res = NULL,
                            age_res = NULL,
                            cache_dir = NULL,
                            enforce_cache = FALSE){

  # Check inputs
  join <- check_res_args(time_res = time_res,
                         spat_res = spat_res,
                         age_res = age_res)
  check_enforce_cache(enforce_cache)

  # set up stuff for cacheing
  filename <- "vaccination_processed.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 1, units = "days")

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
      dat <- process_vaccination(cache_dir = cache_dir,
                                 filename = filename,
                                 enforce_cache = enforce_cache)
    }
  }

  # aggregate if desired
  if(join){
    if(spat_res != 1L || time_res != "daily" || age_res != "age"){

      # get desired resolutions
      dims <- get_case_info(spat_res = spat_res, time_res = time_res, cache_dir = cache_dir)

      # aggregate over age groups and space if desired
      if(spat_res < 1L || age_res == "no_age"){
        # get population for weights if spat_res is larger than native resolution or we need to aggregate over age groups
        pop <- get_population(time_res = "daily", spat_res = 1L, age_res = "age")
        dat <- dat %>%
          dplyr::left_join(pop, by = c("region", "date", "age"),
                           suffix = c(".vac", ".pop")) %>%
          {
            if(age_res == "no_age"){
              .[] %>% dplyr::mutate(age = "total")
            } else .
          } %>%
          {
            if(spat_res < 1L){
              .[] %>% dplyr::mutate(region = substr(region, 1, spat_res + 2L))
            } else .
          } %>%
          dplyr::group_by(region, age, date) %>%
          dplyr::summarise(value = stats::weighted.mean(x = value.vac, w = value.pop), .groups = "drop")
      }

      # aggregate over time (just pick the values that are in the time aggregated case dates)
      if(time_res == "weekly")
        dat <- dat %>% dplyr::filter(date %in% dims$date)

      # if spatial resolution is higher NUTS level than native, expand
      if(spat_res > 1L){
        grid <- tidyr::expand_grid(age = if(age_res == "age") dims$age else "total",
                                   date = dims$date,
                                   region = dims$region)
        dat <- grid %>%
          dplyr::mutate(region1 = substr(region, 1, 3L)) %>%
          dplyr::left_join(dat, by = c("age" = "age", "date" = "date", "region1" = "region")) %>%
          dplyr::select(-region1)
        # Fill with ones before campaign started
        idx <- which(!is.na(dat$value))[1]
        if(idx != 1L) dat$value[1:idx] <- 1
      }

    }
  }

  # take the log
  dat <- dplyr::mutate(dat, value = log(value))

  dat
}


#' Process vaccination data and map it to the age groups in the case data set
#'
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{tibble} with the proportion of unvaccinated cases stratified by age group, date and region.
#' However, the age groups here correspond to those in the case data set.
#' @noRd
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate group_by summarise left_join arrange ungroup select right_join distinct rowwise bind_rows rename
process_vaccination <- function(cache_dir,
                                filename,
                                enforce_cache){

  # get population and vaccination
  pop <- get_one_year_population(cache_dir = cache_dir,
                                 enforce_cache = enforce_cache)

  vac <- get_raw_vaccination(cache_dir = cache_dir,
                             enforce_cache = enforce_cache)

  # Join aggregated population to vaccination data
  vac <- vac %>%
    # aggregate to NUTS 1
    dplyr::mutate(region = substr(region, 1, 3)) %>%
    dplyr::group_by(region, age, date) %>%
    dplyr::summarise(value = sum(value), .groups = "drop") %>%
    # Join population
    dplyr::left_join(pop %>%
                       dplyr::group_by(region, age_group_vac) %>%
                       dplyr::summarise(value = sum(value), .groups = "drop"),
                     suffix = c(".vac", ".pop"), by = c("region" = "region", "age" = "age_group_vac")) %>%
    # Get cumulative counts
    dplyr::group_by(region, age) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(value.vac = cumsum(value.vac)) %>%
    dplyr::ungroup() %>%
    # Calculate proportion of unvaccinated
    dplyr::mutate(value = 1 - value.vac / value.pop) %>%
    # Test that proportion is between 0 and 1
    {
      if(any(.$value < 0)) stop("Proportion of vaccinated people is larger than 1.")
      .
    } %>%
    dplyr::select(-value.vac, -value.pop) %>%
    # Add age groups of case data set
    dplyr::right_join(pop %>%
                        dplyr::select(age_group_vac, age_group_case) %>%
                        dplyr::distinct(),
                      by = c("age" = "age_group_vac")) %>%
    dplyr::rename(age_group_vac = age) %>%
    # Add start age and end age
    dplyr::mutate(age_vac_start = as.numeric(sub("^(\\d+)[-+].*$", "\\1", age_group_vac)),
                  age_vac_end = as.numeric(ifelse(grepl("\\+$", age_group_vac), "100", sub("^\\d+-(\\d+)$", "\\1", age_group_vac))),
                  age_case_start = as.numeric(sub("^(\\d+)[-+].*$", "\\1", age_group_case)),
                  age_case_end = as.numeric(ifelse(grepl("\\+$", age_group_case), "100", sub("^\\d+-(\\d+)$", "\\1", age_group_case)))) %>%
    # compute which observations need to be averaged
    dplyr::rowwise() %>%
    dplyr::mutate(case_contained = all(seq(age_case_start, age_case_end, 1L) %in% seq(age_vac_start, age_vac_end, 1L))) %>%
    dplyr::ungroup()

  # For those age groups in the case data set that span multiple age groups in the vaccination data,
  # Calculate weighted average of the proportion of unvaccinated (use population weights)
  weighted_groups <- vac %>%
    dplyr::filter(!case_contained) %>%
    dplyr::rowwise() %>%
    # Compute which ages we need from the vaccination age groups
    dplyr::mutate(ages = list(seq(age_vac_start, age_vac_end, 1L)[seq(age_vac_start, age_vac_end, 1L) %in% seq(age_case_start, age_case_end, 1L)])) %>%
    dplyr::ungroup() %>%
    # Unnest list column
    tidyr::unnest("ages") %>%
    # Join population data
    dplyr::left_join(pop %>% dplyr::select(age, region, value),
                     by = c("region" = "region", "ages" = "age"),
                     suffix = c(".prop_unvac", ".pop")) %>%
    # Calculate weighted average for each region, age group and day
    dplyr::group_by(region, age_group_case, date) %>%
    dplyr::summarise(value = stats::weighted.mean(x = value.prop_unvac,
                                                  w = value.pop), .groups = "drop")

  # Combine both
  vac <- vac %>%
    dplyr::filter(case_contained) %>%
    dplyr::select(region, age_group_case, date, value) %>%
    dplyr::bind_rows(weighted_groups) %>%
    dplyr::rename(age = age_group_case) %>%
    dplyr::arrange(region, date, age)

  # Save the preprocessed file
  saveRDS(vac, file = make_path(cache_dir, filename))

  #return
  vac

}


#' Download the vaccination data set from the RKI Github repository or get a cached version of the data
#'
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{data.frame} containing the number of people who received the first dose of
#' vaccination against COVID-19 stratified by age group, date and Bundesland.
#'
#' @noRd
get_raw_vaccination <- function(cache_dir,
                                enforce_cache){

  # Check inputs
  check_enforce_cache(enforce_cache)

  # set parameters for cacheing
  filename <- "raw_vaccination.rds"

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 1, units = "days")

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
      dat <- get_raw_vaccination_from_source(cache_dir, filename)
    }
  }

  dat
}

#' Download the vaccination data set from the RKI Github repository
#'
#' @template cache_dir
#' @param filename Name of the file, where output should be saved
#'
#' @return A \code{data.frame} containing the number of people who received the first dose of
#' vaccination against COVID-19 stratified by age group, date and Bundesland.
#' @noRd
#' @importFrom dplyr bind_rows bind_cols tibble filter left_join group_by summarise select rename filter mutate
#' @importFrom readr read_csv
#' @importFrom tidyr expand_grid
get_raw_vaccination_from_source <- function(cache_dir, filename){

  # Get the table that maps Landkreise to NUTS levels
  nuts <- nuts_table(cache_dir = cache_dir)
  ## Add Eisenach to Wartburgkreis (as Eisenach is not used in the case data set)
  nuts <- dplyr::bind_rows(
    nuts,
    dplyr::bind_cols(dplyr::tibble(LK = "SK Eisenach", adm_unit = "16056"),
                     nuts[nuts$adm_unit == "16063", 3:ncol(nuts)])
  )

  # read vaccination data
  dat <- "https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv" %>%
    readr::read_csv(col_types = readr::cols(LandkreisId_Impfort = "c",
                                            Altersgruppe = "c",
                                            Impfschutz = "i",
                                            Anzahl = "i",
                                            Impfdatum = "D"),
                    show_col_types = FALSE,
                    progress = FALSE) %>%
    dplyr::filter(Impfschutz == 1L) %>%
    # Add nuts regions
    dplyr::left_join(nuts, by = c("LandkreisId_Impfort" = "adm_unit")) %>%
    {
      unmapped_lk <- .[] %>% filter(is.na(lvl3)) %>% pull(LandkreisId_Impfort) %>% unique()
      if(length(setdiff(unmapped_lk, c("17000", "u"))) != 0L) stop("Unexpected unmapped Landkreis detected.")
      .[.$LandkreisId_Impfort %in% unmapped_lk, 6:ncol(.)] <- "unknown"
      .
    } %>%
    # Sum up number of vaccines for each of the groups (date, region, age)
    dplyr::group_by(dplyr::across(c(-LandkreisId_Impfort, -Impfschutz, -Anzahl))) %>%
    dplyr::summarise(Anzahl = sum(Anzahl), .groups = "drop") %>%
    # Keep only what is needed
    dplyr::select(Impfdatum, Altersgruppe, lvl3, Anzahl) %>%
    dplyr::rename(date = Impfdatum, age = Altersgruppe, region = lvl3, value = Anzahl) %>%
    dplyr::filter(region != "unknown")

  # Complete the time series
  contains_youngest <- any(grepl("04", unique(dat$age), fixed = TRUE))
  grid <- tidyr::expand_grid(region = unique(dat$region),
                             age = if(contains_youngest) unique(dat$age) else c("00-04", unique(dat$age)),
                             date = seq(min(dat$date), max(dat$date), 1L))
  dat <- dplyr::left_join(grid, dat, by = c("date", "age", "region")) %>%
    dplyr::mutate(value = ifelse(is.na(value), 0L, value))

  # save this to cache dir
  saveRDS(dat, file = make_path(cache_dir, filename))

  dat
}

#' Download the one year age population from Eurostat or get a cached version of the same data
#'
#' @template cache_dir
#' @template enforce_cache
#'
#' @return A \code{data.frame} containing the number of inhabitants stratified by Bundesland
#' and one year age bands. The output also contains columns that indicate the respective
#' age group that people of a specific age belong to in the vaccination and case data set, respectively.
#' @noRd
get_one_year_population <- function(cache_dir,
                                    enforce_cache){

  # Check inputs
  check_enforce_cache(enforce_cache)

  # set parameters for cacheing
  filename <- "one_year_population.rds"

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
      dat <- get_one_year_population_from_source(cache_dir = cache_dir, filename = filename)
    }
  }

  dat

}

#' Download the one year age population from Eurostat
#'
#' @param filename Name of the file, where output should be saved
#' @template cache_dir
#'
#' @return A \code{data.frame} containing the number of inhabitants stratified by Bundesland
#' and one year age bands. The output also contains columns that indicate the respective
#' age group that people of a specific age belong to in the vaccination and case data set, respectively.
#' @noRd
#' @importFrom restatapi get_eurostat_data
#' @importFrom dplyr filter select mutate case_when rename
get_one_year_population_from_source <- function(cache_dir, filename){

  # get population per Bundesland and age group
  pop <- suppressWarnings(restatapi::get_eurostat_data("demo_r_d2jan",
                                      stringsAsFactors = FALSE,
                                      cache = FALSE,
                                      verbose = FALSE)) %>%
    dplyr::filter(sex == "T",
                  time == "2020",
                  #grepl("^DE[1-9A-Z]{2}$", geo),
                  grepl("^DE[1-9A-Z]{1}$", geo),
                  !(age %in% c("UNK", "TOTAL"))) %>%
    dplyr::select(age, geo, values) %>%
    dplyr::mutate(age = gsub("^Y_?", "", age),
                  age = dplyr::case_when(age == "OPEN" ~ "100",
                                         age == "LT1" ~ "0",
                                         TRUE ~ age),
                  age = as.numeric(age),
                  age_group_vac = as.character(cut(age,
                                               breaks = c(0, 4, 11, 17, 59, Inf),
                                               include.lowest = TRUE,
                                               right = TRUE)),
                  age_group_vac = prettify_labels(age_group_vac),
                  age_group_case = as.character(cut(age,
                                                    breaks = c(0, 4, 14, 34, 59, 79, Inf),
                                                    include.lowest = TRUE,
                                                    right = TRUE)),
                  age_group_case = prettify_labels(age_group_case)) %>%
    dplyr::rename(region = geo, value = values)

  # save this to file
  saveRDS(pop, file = make_path(cache_dir, filename))

  pop
}


#' Construct labels from the default labels used by the \code{\link[base]{cut}} function
#'
#' @param labels A vector of labels as constructed by \code{\link[base]{cut}}.
#'
#' @return A character vector with prettier labels
#' @noRd
#'
prettify_labels <- function(labels){
  lab <- do.call(`rbind`, strsplit(labels, split = ",", fixed = TRUE))
  lab <- vapply(list(first = 1L, second = 2L), function(x){
    out <- gsub("\\D", "", lab[, x])
    if(x == 1L){
      change <- !startsWith(lab[, x], "[")
      out[change] <- as.character(as.integer(out[change]) + 1L)
    } else {
      change <- !endsWith(lab[, x], "]") & nchar(out) != 0
      out[change] <- as.character(as.integer(out[change]) - 1L)
      out[nchar(out) == 0L] <- "+"
    }
    ifelse(nchar(out) == 1L & out != "+", paste0("0", out), out)
  }, character(length(labels)))
  paste0(lab[, 1L], ifelse(lab[, 2L] != "+", "-", ""), lab[, 2L])
}

