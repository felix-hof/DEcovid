% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variants.R
\name{get_variants}
\alias{get_variants}
\title{Get data on dominant SARS-CoV-2 variants in Germany}
\usage{
get_variants(
  time_res = NULL,
  spat_res = NULL,
  age_res = NULL,
  cache_dir = NULL,
  enforce_cache = FALSE
)
}
\arguments{
\item{time_res}{A \code{character} vector of length 1 indicating the temporal resolution of the data.
Accepted values are either \code{"daily"} or \code{"weekly"}.}

\item{spat_res}{A \code{numeric} vector of length 1 indicating the spatial resolution of the data.
Accepted values are \code{0}, \code{1}, \code{2} and \code{3}. Corresponds to the respective NUTS level.}

\item{age_res}{A \code{character} vector of length 1 indicating whether or not the data should be stratified by
age groups. Accepted values are \code{"age"} and \code{"no_age"}.}

\item{cache_dir}{This argument indicates the path to the
desired cache directory. Defaults to \code{NULL}.
The desired cache directory is determined according to the following hierarchy:
\enumerate{
\item{If \code{cache_dir} defined by user: Caches files to user defined directory.}
\item{If \code{cache_dir == NULL} (default): check if \code{getOption("DEcovid_cache_dir")} is
defined and use this as cache directory.}
\item{If \code{getOption("DEcovid_cache_dir")} undefined: Use \code{tempdir()} as
cache directory.}
}
If you would like to always use the same cache directory, the simplest way to do so is
to run the following line of code: \code{options(DEcovid_cache_dir = "path/to/your/cache/directory")}.
Afterwards you can just ignore the \code{cache_dir} argument as it always will use the path you set in
the options.}

\item{enforce_cache}{Either \code{TRUE} or \code{FALSE} (default). If \code{TRUE}, a cached version
of the data set is loaded. If there is no cached version, the function returns an error. If \code{FALSE},
the function returns a cached version if the data set is not older than a usual update cycle of the remote
source. Otherwise, the data is fetched from remote sources.}
}
\value{
A \code{list} whose elements are time series containing the relative prevalence for each of the SARS-CoV-2 variants that
were dominant at some point in time.
}
\description{
The function accesses data from \insertCite{DEcovid:VARdata;textual}{DEcovid} and calculates the relative prevalence for
each of the variants that were dominant at some point in time. If this is desired, the data is extended to match the desired resolution. The
calculations within the functions are done by dividing the number of cases for each variant by the total number of people sequenced. If there
was no one sequenced within a specific week, the relative prevalence is assumed to be 0 for all virus variants.
}
\examples{
variants <- get_variants()
variants <- get_variants(time_res = "daily", spat_res = 1, age_res = "no_age")
}
\references{
\insertRef{DEcovid:VARdata}{DEcovid}
}
