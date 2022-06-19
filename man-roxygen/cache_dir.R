#' @param cache_dir This argument indicates the path to the
#' desired cache directory. Defaults to \code{NULL}.
#' The desired cache directory is determined according to the following hierarchy:
#' \enumerate{
#'   \item{If \code{cache_dir} defined by user: Caches files to user defined directory.}
#'   \item{If \code{cache_dir == NULL} (default): check if \code{getOption("DEcovid_cache_dir")} is
#'   defined and use this as cache directory.}
#'   \item{If \code{getOption("DEcovid_cache_dir")} undefined: Use \code{tempdir()} as
#'   cache directory.}
#' }
#' If you would like to always use the same cache directory, the simplest way to do so is
#' to run the following line of code: \code{options(DEcovid_cache_dir = "path/to/your/cache/directory")}.
#' Afterwards you can just ignore the \code{cache_dir} argument as it always will use the path you set in
#' the options.
