% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_data}
\alias{get_data}
\title{Load all possible data sets depending on the resolutions desired}
\usage{
get_data(spat_res, time_res, age_res, ...)
}
\arguments{
\item{spat_res}{A \code{numeric} vector of length 1 indicating the spatial resolution of the data.
Accepted values are \code{0}, \code{1}, \code{2} and \code{3}. Corresponds to the respective NUTS level.}

\item{time_res}{A \code{character} vector of length 1 indicating the temporal resolution of the data.
Accepted values are either \code{"daily"} or \code{"weekly"}.}

\item{age_res}{A \code{character} vector of length 1 indicating whether or not the data should be stratified by
age groups. Accepted values are \code{"age"} and \code{"no_age"}.}

\item{...}{Further arguments passed to the functions \code{get_xx} where \code{xx} is a placeholder for any of the
functions returning data sets within this package.}
}
\value{
A \code{list} containing all of the data sets that could possibly be used for an Endemic-Epidemic model
with the specified resolutions.
}
\description{
This function is a wrapper around the various functions that load data sets within this package.
It allows the user to define the spatial, temporal and age group resolutions as well as further arguments for all
of the functions that download the data sets and receive a list with all of the possible data sets for the desired
resolutions.
}
\details{
Internally, this function calls \code{data("overview", package = "DEcovid")} to determine whether a
data set is suitable for the specified resolutions. Of course, downloading all of the data sets for the first
time will take some time.
}
\examples{
\dontrun{
spat_res <- 0
time_res <- "weekly"
age_res <- "no_age"

data <- get_data(age_res = age_res, spat_res = spat_res, time_res = time_res,
                 complete = "region", enforce_cache = FALSE)
}
}
