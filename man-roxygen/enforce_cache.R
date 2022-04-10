#' @param enforce_cache Either \code{TRUE} or \code{FALSE} (default). If \code{TRUE}, a cached version
#' of the data set is loaded. If there is no cached version, the function returns an error. If \code{FALSE},
#' the function returns a cached version if the data set is not older than a usual update cycle of the remote
#' source. Otherwise, the data is fetched from remote sources.
