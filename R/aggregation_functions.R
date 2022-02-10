# cases
time_f_cases <- sum
spat_f_cases <- sum
age_f_cases <- sum

# area_size
time_f_area_size <- function(x){x[!is.na(x)][1]}
spat_f_area_size <- sum
age_f_area_size <- function(x){x[!is.na(x)][1]}

# holidays
time_f_holidays <- sum
spat_f_holidays <- function(x) sum(x, na.rm = TRUE) / length(x)
age_f_holidays <- function(x){x[!is.na(x)][1]}

# vaccination
time_f_vaccination <- function(x) log(mean(exp(x), na.rm = TRUE))
spat_f_vaccination <- function(x) x[!is.na(x)][1]
age_f_vaccination <- function(x) x[!is.na(x)][1]

# testing
time_f_testing <- function(x) mean(x, na.rm = TRUE)
spat_f_testing <- function(x) x[!is.na(x)][1]
age_f_testing <- function(x) x[!is.na(x)][1]

# stringency
time_f_stringency <- function(x) mean(x, na.rm = TRUE)
spat_f_stringency <- function(x) x[!is.na(x)][1]
age_f_stringency <- function(x) x[!is.na(x)][1]

# population
time_f_population <- function(x) x[!is.na(x)][1]
spat_f_population <- sum
age_f_population <- sum

# temperature
time_f_temperature <- function(x) mean(x, na.rm = TRUE)
spat_f_temperature <- function(x) mean(x, na.rm = TRUE)
age_f_temperature <- function(x) x[!is.na(x)][1]

#' Aggregate data sets across time, space and age groups
#'
#' @param data A \code{tibble} with columns \code{region}, \code{date}, \code{age} and \code{value}
#' which should be aggregated.
#' @param time_res The temporal resolution to which the data should be aggregated. Possible values are
#' \code{"daily"} and \code{"weekly"}.
#' @param spat_res The NUTS level to which the data should be aggregated. Possible values are
#' \code{0}, \code{1}, \code{2} and \code{3}. These numbers correspond to the NUTS level.
#' @param age_res Whether or not the data should be aggregated. Possible values are
#' \code{"age"} and \code{"no_age"}. The latter aggregates the data over age groups.
#' @param time_f A function used to aggregate along the temporal dimension.
#' @param spat_f A function used to aggregate along the spatial dimension.
#' @param age_f A function used to aggregate the age groups.
#'
#' @return A \code{tibble} with columns \code{region}, \code{date}, \code{age} and \code{value}.
#'
#' @importFrom dplyr %>% mutate group_by summarise across
#' @importFrom ISOweek ISOweek2date date2ISOweek
#'
summarise_data <- function(data,
                           time_res, spat_res, age_res,
                           time_f, spat_f, age_f){
  data %>%
    # aggregate by region
    {
      if(spat_res != 3){
        if(nchar(.$region[1]) > spat_res + 2L){
          .[] %>%
            dplyr::mutate(region = substr(region, 1, spat_res + 2)) %>%
            dplyr::group_by(dplyr::across(c(-value))) %>%
            dplyr::summarise(value = spat_f(value), .groups = "drop")
        } else {
          .
        }
      } else {
        .
      }
    } %>%
    # aggregate by time
    {
      if(time_res != "daily"){
        if(all(diff(unique(.$date)) == 7L)){
          .
        } else {
          .[] %>%
            dplyr::mutate(date = gsub("-\\d$", "-4", ISOweek::date2ISOweek(date))) %>%
            dplyr::group_by(dplyr::across(c(-value))) %>%
            dplyr::summarise(value = time_f(value), .groups = "drop") %>%
            dplyr::mutate(date = ISOweek::ISOweek2date(date))
        }
      } else {
        .
      }
    } %>%
    # aggregate by age
    {
      if(age_res != "age"){
        if(length(unique(.$age)) != 1L){
          .[] %>%
            dplyr::mutate(age = "total") %>%
            dplyr::group_by(dplyr::across(c(-value))) %>%
            dplyr::summarise(value = age_f(value), .groups = "drop")
        } else {
          .
        }
      } else {
        .
      }
    }
}
