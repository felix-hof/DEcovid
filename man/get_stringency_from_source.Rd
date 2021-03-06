% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringency.R
\name{get_stringency_from_source}
\alias{get_stringency_from_source}
\title{Get stringency data from OxCGRT}
\usage{
get_stringency_from_source(cache_dir, filename)
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

\item{filename}{The name of the file where the stringency data is stored.}
}
\value{
A \code{tibble} with columns \code{date}  and \code{value} (contains stringency index).
}
\description{
Get stringency data from OxCGRT
}
