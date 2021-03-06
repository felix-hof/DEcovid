% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nuts_table.R
\name{nuts_table}
\alias{nuts_table}
\title{Get the NUTS table used to map German national statistical units to NUTS regions}
\usage{
nuts_table(cache_dir = NULL, enforce_cache = FALSE)
}
\arguments{
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
A \code{tibble} mapping German national administrative to the NUTS classification.
}
\description{
This function returns the mapping from German Landkreise to the NUTS system.
Internally, it uses data from \insertCite{DEcovid:RKINUTS;textual}{DEcovid} and \insertCite{DEcovid:NUTS;textual}{DEcovid}.
}
\examples{
nts <- nuts_table()

}
\references{
\insertRef{DEcovid:RKINUTS}{DEcovid}
\insertRef{DEcovid:NUTS}{DEcovid}
}
