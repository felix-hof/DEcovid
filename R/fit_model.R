#' Convert data tibble to matrix for use with Surveillance-package
#'
#' @param data A \code{tibble} containing the columns \code{age}, \code{date}, \code{value} \code{region}.
#'
#' @return A matrix that can be used as a covariate in \code{hhh4} models or to fill the \code{observed} slot
#' in an \code{sts}-object.
#'
#' @importFrom dplyr group_by summarise mutate select pull
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr set_rownames
#' @export
#'
covariate_matrix <- function(data){

  # input checks
  cond <- all(data$age == "total") || all(grepl("^\\d{2}[-+]\\d*$", data$age))
  if(!cond){
    stop("There is an unknown age group in the data.")
  }
  if(!all(c("age", "date", "value", "region") %in% colnames(data))){
    stop("Data must have columns 'age', 'date', 'value' and 'region'.")
  }
  if(length(unique(diff(as.numeric(unique(data$date))))) != 1L){
    stop("Differences between dates must be regular.")
  }
  check <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(n_region = length(region),
              n_age = length(age),
              .groups = "drop")
  if(!all(check$n_region == check$n_region[1])){
    stop("There must be the same number of regions on each day.")
  }
  if(!all(check$n_age == check$n_age[1])){
    stop("There must be the same number of age groups on each day.")
  }

  # convert tibble to matrix for use with surveillance
  data %>%
    {
      if(check$n_region[1] == 1L & check$n_age[1] == 1L){
        .[] %>% dplyr::mutate(name = region) %>% dplyr::select(date, name, value)
      } else if (check$n_region[1] == 1L & check$n_age[1] != 1L){
        .[] %>% dplyr::mutate(name = age) %>% dplyr::select(date, name, value)
      } else if (check$n_region[1] != 1L & check$n_age[1] == 1L){
        .[] %>% dplyr::mutate(name = region) %>% dplyr::select(date, name, value)
      } else {
        .[] %>% dplyr::mutate(name = paste0(region, ".", age)) %>% dplyr::select(date, name, value)
      }
    } %>%
    tidyr::pivot_wider(id_cols = date, names_from = name, values_from = value) %>%
    {
      rnms <- .[] %>% dplyr::pull(date) %>% as.character()
      .[] %>%
        dplyr::select(-date) %>%
        as.matrix() %>%
        magrittr::set_rownames(rnms)
    }
}
