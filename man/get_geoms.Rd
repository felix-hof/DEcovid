% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoms.R
\name{get_geoms}
\alias{get_geoms}
\title{Get geometries for Germany and its neighbour countries}
\usage{
get_geoms(cache_dir = NULL, enforce_cache = FALSE)
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
A \code{list} of length 5. All elements of this list are of class \code{sf}. The first four elements
contain geometries of German NUTS regions on levels 0-3, whereas the fifth elements contains
geometries of Germany's neighbour countries, where France's overseas territories have been cut off.
All of the geometries also contain a column \code{color} that contains a color
index such that no neighbouring regions have the same color. This column is constructed by an internal call to
\code{\link[DEcovid]{get_map_colors}}.
}
\description{
This function downloads shapefiles from \insertCite{DEcovid:ESgeoms;textual}{DEcovid} and creates processes the data
such that the geometries can be used to create maps. The output of the function is also used to compute symmetric neighbourhood order
matrices in \code{\link[DEcovid]{get_nb_matrix}}.
}
\examples{
geoms <- get_geoms()

}
\references{
\insertRef{DEcovid:ESgeoms}{DEcovid}
}
