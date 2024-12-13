#' @inherit chron::chron_trans
#' @inheritParams chron::scale_x_chron
#' @examples
#' # example code
#' if(require("ggplot2") && requireNamespace("chron", quietly = TRUE)) {
#'     dd <- data.frame(tt = chron::chron(1:10), value = 101:110)
#'     p <- ggplot(dd, aes(tt, value)) +
#'              geom_point() + scale_x_chron(format = "%m-%d")
#'     print(p)
#' }
#' @rdname chron_scale
#' @details
#' It is based on \code{\link[chron]{chron_trans}} and \code{\link[chron]{scale_x_chron}} and \code{\link[chron]{scale_y_chron}} from the \code{chron} package written by \code{chron} package (version 2.3.61) author and maintainer.
#'
#' These functions in the \code{chron} package have a bug that causes the date and time to be out of sync with the time zone if the time zone is not \code{"GMT"} or \code{"UTC"}.
#' These functions in the \code{infunx} package fix the above bug.
#' @export
chron_trans <- function(format = "%Y-%m-%d", n = 5){
  if(!requireNamespace("chron", quietly = TRUE)) stop("This function will not work unless the `{chron}` package is installed")
  if(!requireNamespace("scales", quietly = TRUE)) stop("This function will not work unless the `{scales}` package is installed")

  breaks. <- function(x) chron::chron((scales::pretty_breaks(n))(x))
  format. <- function(x) format(as.POSIXlt(x, tz = "GMT"),
                                format = format)
  scales::trans_new("chron", transform = as.numeric, inverse = chron::chron,
                    breaks = breaks., format = format.)
}

#' @rdname chron_scale
#' @export
scale_x_chron <- function(..., format = "%Y-%m-%d", n = 5){
  if(!requireNamespace("ggplot2", quietly = TRUE)) stop("This function will not work unless the `{ggplot2}` package is installed")

  ggplot2::scale_x_continuous(..., trans = chron_trans(format, n))
}

#' @rdname chron_scale
#' @export
scale_y_chron <- function (..., format = "%Y-%m-%d", n = 5){
  if(!requireNamespace("ggplot2", quietly = TRUE)) stop("This function will not work unless the `{ggplot2}` package is installed")

  ggplot2::scale_y_continuous(..., trans = chron_trans(format, n))
}
