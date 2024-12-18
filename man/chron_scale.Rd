% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chron_scale.R
\name{chron_trans}
\alias{chron_trans}
\alias{scale_x_chron}
\alias{scale_y_chron}
\title{Coordinates and Positions for Chronological Objects}
\usage{
chron_trans(format = "\%Y-\%m-\%d", n = 5)

scale_x_chron(..., format = "\%Y-\%m-\%d", n = 5)

scale_y_chron(..., format = "\%Y-\%m-\%d", n = 5)
}
\arguments{
\item{format}{format string as described in \code{\link[base]{strptime}}.}

\item{n}{Approximate number of axis ticks.}

\item{...}{Passed to \code{\link[ggplot2]{scale_x_continuous}}.}
}
\description{
\code{chron_trans} is a \pkg{ggplot2} transformer for \code{chron}.
  \code{scale_x_chron} and \code{scale_y_chron} are \pkg{ggplot2} scales.
}
\details{
It is based on \code{\link[chron]{chron_trans}} and \code{\link[chron]{scale_x_chron}} and \code{\link[chron]{scale_y_chron}} from the \code{chron} package written by \code{chron} package (version 2.3.61) author and maintainer.

These functions in the \code{chron} package have a bug that causes the date and time to be out of sync with the time zone if the time zone is not \code{"GMT"} or \code{"UTC"}.
These functions in the \code{infunx} package fix the above bug.
}
\examples{
# example code
if(require("ggplot2") && requireNamespace("chron", quietly = TRUE)) {
    dd <- data.frame(tt = chron::chron(1:10), value = 101:110)
    p <- ggplot(dd, aes(tt, value)) +
             geom_point() + scale_x_chron(format = "\%m-\%d")
    print(p)
}
}
