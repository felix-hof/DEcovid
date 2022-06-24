#' Functionality used for the master thesis of Felix Hofmann
#'
#' The package \pkg{DEcovid} provides functions used in the author's thesis.
#' Some of the functions are used to fetch data sets, whereas others provide
#' the functionality to process the data such that it can be used with the
#' \code{hhh4} model class implemented in the
#' \code{\link[surveillance]{surveillance-package}}.
#'
#' @section Functions included:
#' For data fetching and preprocessing: \cr
#' \cr
#' \code{\link{get_cases}} \cr
#' \code{\link{nuts_table}} \cr
#' \code{\link{get_boundary_inds}} \cr
#' \code{\link{get_area_size}} \cr
#' \code{\link{get_geoms}} \cr
#' \code{\link{get_map_colors}} \cr
#' \code{\link{get_temperature}} \cr
#' \code{\link{get_holidays}} \cr
#' \code{\link{get_population}} \cr
#' \code{\link{get_stringency}} \cr
#' \code{\link{get_testing}} \cr
#' \code{\link{get_urbanicity}} \cr
#' \code{\link{get_vaccination}} \cr
#' \code{\link{get_variants}} \cr
#' \code{\link{get_weekday}} \cr
#' \code{\link{get_weekend}} \cr
#' \cr
#' For generation of tables and plots: \cr
#' \cr
#' \code{\link{parameter_table}} \cr
#' \cr
#' For reshaping data between tibbles and
#' matrices used in \code{\link[surveillance]{hhh4}} or \code{\link[hhh4addon]{profile_par_lag}}: \cr
#' \cr
#' \code{\link{df2matrix}} \cr
#' \code{\link{matrix2df}} \cr
#' \cr
#' For generating a grid of formulas for use with \code{\link[surveillance]{hhh4}} or \code{\link[hhh4addon]{profile_par_lag}} from a selection of covariates: \cr
#' \cr
#' \code{\link{make_formulas}} \cr
#'
#' @docType package
#' @name DEcovid
NULL
