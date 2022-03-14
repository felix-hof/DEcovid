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
                     n_unique_regions = length(unique(region)),
                     n_unique_age = length(unique(age)),
                     .groups = "drop")
  if(!all(check$n_region == check$n_region[1])){
    stop("There must be the same number of regions on each day.")
  }
  if(!all(check$n_age == check$n_age[1])){
    stop("There must be the same number of age groups on each day.")
  }
  if(!all(check$n_unique_regions == check$n_unique_regions[1])){
    stop("There must be the same amount of unique regions on each day.")
  }
  if(!all(check$n_unique_age == check$n_unique_age[1])){
    stop("There must be the same amount of unique age groups on each day.")
  }

  # convert tibble to matrix for use with surveillance
  data %>%
    {
      if(check$n_unique_regions[1] == 1L & check$n_unique_age[1] == 1L){
        .[] %>% dplyr::mutate(name = region) %>% dplyr::select(date, name, value)
      } else if (check$n_unique_regions[1] == 1L & check$n_unique_age[1] != 1L){
        .[] %>% dplyr::mutate(name = age) %>% dplyr::select(date, name, value)
      } else if (check$n_unique_regions[1] != 1L & check$n_unique_age[1] == 1L){
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

#' Construct formulas for all \code{hhh4} model components
#'
#' @param end Either \code{NULL} or \code{character} vector listing all covariates that should be tried in the endemic component.
#' @param epi Either \code{NULL} or \code{character} vector listing all covariates that should be tried in the epidemic component.
#' @param ar Either \code{NULL} or \code{character} vector listing all covariates that should be tried in the autoregressive component.
#' @param period Either \code{NULL} or the period that is passed to \code{\link[surveillance]{addSeason2formula}}.
#' @param restrict Either \code{NULL} or an object of class \code{list} containing restrictions on the covariates passed in arguments \code{end}, \code{epi},
#' and \code{ar}. If not \code{NULL}, the \code{list} can have elements of  named "end", "epi" or "ar" which are again of class \code{list}
#' and determine restrictions to be applied to covariate combinations within each of the components. Restrictions must be stated
#' in the following form:
#' \describe{
#' \item{combined}{A \code{list} whose elements are \code{character} stating which covariates should be considered as one combined
#' covariate. This is useful in case of mutually exclusive indicators like weekday effects as it restricts the resulting formulas to
#' either contain all indicators or none of them.}
#' \item{always}{A \code{character} vector containing covariates that must always be included in the formula.}
#' \item{exclusive}{A \code{list} whose elements are again \code{lists} that contain mutually exclusive covariates. This
#' is useful as it allows the resulting formulas to contain maximally one of the covariates listed in these list elements.}
#' }
#'
#' For further clarification on how to construct the restrictions list, see the examples section.
#' @return A \code{data.frame} containing all possible combinations of formulas given model components and restrictions on
#' covariates.
#' @export
#'
#' @examples
#' weekdays <- paste0("wkd_", c("tu", "we", "th", "fr", "sa", "su"))
#' end <- c("vaccination", "temperature", "seasonality", "testing",
#'          "fe(1, unitSpecific = TRUE)", weekdays)
#' epi <- c("seasonality2", "testing", "stringency", "ri()", weekdays)
#' ar <- NULL
#' restrict <- list(end = list(combined = list(weekdays, c("testing", "vaccination")),
#'                             always = c("vaccination", weekdays),
#'                             exclusive = list(list(weekdays, "seasonality"),
#'                                              list("temperature", "seasonality"))),
#'                  epi = list(combined = list(weekdays),
#'                             always = c("seasonality2")))
#' formulas <- make_formulas(end = end, epi = epi, ar = ar,
#'                           period = 52, restrict = restrict)
#'
make_formulas <- function(end = NULL, epi = NULL, ar = NULL, period = NULL, restrict = NULL){

  # input checks for restriction list
  if(!(is.null(restrict) || is.list(restrict))){
    stop("The argument 'restrict' must be either NULL or a list.")
  }
  if(!is.null(restrict)){
    if(!all(names(restrict) %in% c("end", "epi", "ar"))){
      stop("The names of the list in \"restrict\" must be \"end\", \"epi\", and \"ar\".")
    }
  }

  # Subset to only components present in the model
  covariates <- list("end" = end, "epi" = epi, "ar" = ar) # everything from now on depends on this form
  covariates <- covariates[vapply(covariates, function(x) !is.null(x), logical(1L))]
  if(!is.null(restrict)){
    restrict <- restrict[names(covariates)]
  }

  # More input checks
  if(any(!vapply(covariates, is.character, logical(1L)))){
    stop("The arguments \"end\", \"epi\", and \"ar\" must be character vectors.")
  }
  if(any(vapply(covariates, function(x) any(grepl("^seasonality\\d*$", x)), logical(1L))) & is.null(period)){
    stop("If you would like to add seasonality terms in any of the components, the argument \"period\" must be a positive integer.")
  }

  # Sanity check on restrict object
  check_restrict(end = end, epi = epi, ar = ar, restrict = restrict)

  # construct formulas from grids
  comp_grids <- lapply(seq_along(covariates), function(x){
    # get covariates and restrictions for the current component
    restrict_copy <- restrict[[x]]
    cov_copy <- covariates[[x]]
    # replace "combined" covariates by one variable
    for(i in seq_along(restrict_copy$combined)){
      to_replace <- restrict_copy$combined[[i]]
      restrict_copy <- rapply(restrict_copy,
                              function(y){
                                if(any(to_replace %in% y)) unique(replace(y, y %in% to_replace, paste0("Var", i))) else y
                              }, classes = "character", how = "replace")
      cov_copy[cov_copy %in% to_replace] <- paste0("Var", i)
      cov_copy <- unique(cov_copy)
    }
    # expand grid over those covariates that do actually vary
    grid <- expand.grid(lapply(cov_copy[!cov_copy %in% restrict_copy$always], function(...) c(TRUE, FALSE)))
    names(grid) <- cov_copy[!cov_copy %in% restrict_copy$always]
    # add TRUE columns for those covariates that are always in the model
    add <- as.data.frame(matrix(TRUE, nrow = nrow(grid), ncol = length(restrict_copy$always)))
    names(add) <- restrict_copy$always
    grid <- cbind(grid, add)
    # filter
    grid <- apply_restrictions(grid, restrict_copy)
    # reinsert "combined" covariates
    for(i in seq_along(restrict_copy$combined)){
      n <- length(restrict[[x]]$combined[[i]]) - 1L
      add <- as.data.frame(vapply(seq_len(n), function(...) grid[, paste0("Var", i)], logical(nrow(grid))))
      names(add) <- rep(paste0("Var", i), n)
      grid <- cbind(grid, add)
      colnames(grid)[colnames(grid) == paste0("Var", i)] <- restrict[[x]]$combined[[i]]
    }
    f <- grid2formulas(grid = grid, period = period)
    return(f)
  })
  names(comp_grids) <- names(covariates)

  # expand grids to get every combination of formulas
  out <- expand.grid(comp_grids)

  return(out)
}


#' Convert a covariate grid of TRUEs and FALSEs into formulas
#'
#' @param grid A \code{data.frame} with covariate names as column names and entries who take the values TRUE or FALSE.
#' @param period A vector of class \code{integer} and length 1. Must be positive and larger or equal to 1 if set.
#'
#' @details By default, the formulas always contain an intercept. However, adding random or unit-specific intercepts through the functions
#'  \code{\link[surveillance]{ri}} or \code{\link[surveillance]{fe}} automatically removes the global intercept.
#' @return A \code{list} whose elements are formulas containing the covariates specified through the \code{grid}.
#'
#' @importFrom surveillance addSeason2formula
grid2formulas <- function(grid, period = NULL){
  nms <- colnames(grid)
  formulas <- lapply(seq_len(nrow(grid)), function(x){

    # get covariates for the formula
    covs <- colnames(grid)

    # add intercept if not specifically excluded
    if(!any(grepl("^\\s*-\\s*1\\s*", covs))){
      covs <- c("1", nms[unlist(grid[x, ])])
    }

    # handle seasonality (take it out and add it later with function from surveillance package)
    hasSeason <- any(grepl("^seasonality\\d*", covs))
    if(hasSeason & is.null(period)) stop("You added seasonality terms without setting the argument \"period\".")
    if(hasSeason){
      if(sum(grepl("^seasonality\\d*", covs)) > 1L) stop("Seasonality can only be added once to every formula")
      S <- sub("^seasonality", "", covs[grepl("^seasonality\\d*", covs)])
      S <- ifelse(S != "", as.integer(S), 1L)
    }
    covs <- covs[!grepl("^seasonality\\d*", covs)]

    # remove overall intercept if we have unit specific intercepts
    if(any(grepl("^fe\\s*\\(\\s*(x\\s*=\\s*)?1\\s*\\,.+", covs))){
      covs[covs == "1"] <- "-1"
    }

    # remove overall intercept if we have random intercepts
    if(any(grepl("^ri\\s*\\(.+", covs))){
      covs[covs == "1"] <- "-1"
    }

    # make formula
    f <- stats::as.formula(paste0("~", paste(covs, collapse = "+")), env = .GlobalEnv)

    # add seasonality
    if(hasSeason){
      f <- surveillance::addSeason2formula(f, S = S, period = period)
    }
    return(f)
  })
}

#' Filter a grid based on a list of restrictions
#'
#' @param grid A \code{data.frame} filled with TRUE and FALSEs.
#' @param comp_restrict A list with elements:
#'
#' @return The input grid filtered according to the restrictions passed in \code{comp_restrict}.
#'
apply_restrictions <- function(grid, comp_restrict){

  # if(!is.null(comp_restrict[["combined"]])){
  #   idx <- vapply(comp_restrict[["combined"]], function(x){
  #     len <- length(x)
  #     apply(grid[, x, drop = FALSE], 1, function(x) ifelse(sum(x) %in% c(0L, len), TRUE, FALSE))
  #   }, logical(nrow(grid)))
  #   idx <- apply(idx, 1, all)
  #   grid <- grid[idx, ]
  # }

  # if(!is.null(comp_restrict[["always"]])){
  #   idx <- apply(grid[ , comp_restrict[["always"]], drop = FALSE], 1, all)
  #   grid <- grid[idx, ]
  # }

  if(!is.null(comp_restrict[["exclusive"]])){
    idx <- vapply(comp_restrict[["exclusive"]], function(x){
      # calculate whether all of the covariates are in the formula for each of the list components
      idx <- vapply(x, function(y){
        apply(grid[, y, drop = FALSE], 1, all)
      }, logical(nrow(grid)))
      # if there is maximally one (combined) covariate per row, return true else false
      ifelse(rowSums(idx) <= 1L, TRUE, FALSE)
    }, logical(nrow(grid)))
    if(!inherits(idx, "matrix")){
      dim(idx) <- c(length(idx), 1L)
    }
    idx <- apply(idx, 1, all)
    grid <- grid[idx, ]
  }

  return(grid)
}



check_restrict <- function(end, epi, ar, restrict){

  if(!is.list(restrict)){
    stop("The object passed as \"restrict\" argument must be a list.")
  }
  if(!all(names(restrict) %in% c("end", "epi", "ar"))){
    stop("All names of the list passed as \"restrict\" argument must be in c(\"end\", \"epi\", \"ar\").")
  }

  names_ok <- vapply(lapply(restrict, names), function(x) all(x %in% c("combined", "always", "exclusive")), logical(1L))
  if(!all(names_ok)){
    stop("The lists below each component must have names in c(\"combined\", \"always\", \"exclusive\").")
  }

  # Each sublist of each component has correct class
  elements <- c("combined", "always", "exclusive")
  classes <- c("list", "character", "list")
  status <- vapply(seq_along(elements), function(y){
    class_combined <- vapply(lapply(restrict, "[[", i = elements[y]), class, character(1L))
    if(!all(class_combined %in% c(classes[y], "NULL"))){
      stop(paste0("The list element \"", elements[y], "\" under each component must be either NULL or a ",
                  ifelse(classes[y] == "list", "list", "character vector"), "."))
    }
    return(invisible(0L))
  }, integer(1L))

  # Exclusive is a list
  exclusive_class <- vapply(restrict, function(x){ class(x$exclusive) }, character(1L))
  if(!all(exclusive_class %in% c("list", "NULL"))){
    stop("The element \"exclusive\" under each component must be a list.")
  }

  # Check that there are no covariates which are not present in the original components
  all_covs <- unique(unlist(restrict, recursive = TRUE, use.names = TRUE))
  if(!all(all_covs %in% c(end, epi, ar))){
    offending <- all_covs[which(!(all_covs %in% c(end, epi, ar)))]
    stop(paste0("Found covariates in \"restrict\" which are not in any of the passed \"epi\", \"epi\", or \"ar\" components: ",
                paste(offending, collapse = ", ")))
  }

  return(invisible(0L))
}
