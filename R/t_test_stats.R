#' Student's T-test from descriptive statistics.
#' @description
#' Performs one and two sample t-tests from descriptive statistics.
#' In short, it is like \code{python}'s \code{scipy.stats.ttest_ind_from_stats}.
#' This is partially based on the code of \code{\link[stats]{t.test}} in \code{stats} on R 4.2.2 by R Core Team.
#' @param mean_x a mean of sample x.
#' @param std_x a corrected sample standard deviation of sample x.
#' @param n_x a number of observations of sample x.
#' @param mean_y an optional mean of sample y.
#' @param std_y an optional corrected sample standard deviation of sample y.
#' @param n_y an optional number of observations of sample y.
#' @param alternative a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
#' @param var.equal	a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param conf.level confidence level of the interval.
#' @seealso
#' \code{\link[stats]{t.test}}
#' @details
#' For a one-sample only test, the mean and standard deviation of one sample and the number of samples are required.
#' For two-group comparisons, the mean, standard deviation, and number of samples for the two groups are all required.
#' @examples
#' t_test_stats(25, 2.5, 10, mu = 23)
#' t_test_stats(25, 2.5, 10, 23, 3.4, 10)
#' @export

t_test_stats <- function(mean_x, std_x, n_x,
                         mean_y, std_y, n_y,
                         alternative = c("two.sided", "less", "greater"),
                         mu = 0, var.equal = FALSE, conf.level = 0.95){
  alternative <- match.arg(alternative)
  if(length(mean_x) != 1 || length(std_x) != 1 || length(n_x) != 1 ||
     !is.numeric(mean_x) || !is.numeric(std_x) || !is.numeric(n_x)){
    stop("'mean_x' and 'std_x', 'n_x' must be a single number")
  }
  if(!missing(mean_y) || !missing(std_y) || !missing(n_y)){
    if(length(mean_y) != 1 || length(std_y) != 1 || length(n_y) != 1 ||
       !is.numeric(mean_y) || !is.numeric(std_y) || !is.numeric(n_y)){
      stop("'mean_y' and 'std_y', 'n_y' must need a single number")
    }
  }
  if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if(!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                              conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  if(n_x < 2)
    stop("not enough 'x' observations")
  if(!missing(mean_y) && n_y < 2)
    stop("not enough 'y' observations")
  if(std_x < 0)
    stop("'std_x' must be a possiteve single number")
  if(!missing(std_y) && std_y < 0)
    stop("'std_y' must be a possitive single number")

  if(!missing(mean_y)){
    dname <- paste("mean of", deparse(substitute(mean_x)),"and",
                   "mean of", deparse(substitute(mean_y)))
  }else{
    dname <- paste("mean of", deparse(substitute(mean_x)))
  }

  var_x <- std_x ^ 2
  if(missing(mean_y)){
    df <- n_x - 1
    stderr <- sqrt(var_x/n_x)
    if(stderr < 10 *.Machine$double.eps * abs(mean_x))
      stop("data are essentially constant")
    tstat <- (mean_x - mu)/stderr
    method <- "One Sample t-test"
    estimate <- mean_x
    names(estimate) <- "mean of x"
  }else{
    mean_y <- mean_y
    var_y <- std_y ^ 2
    method <- paste(if(!var.equal)
      "Welch", "Two Sample t-test")
    estimate <- c(mean_x, mean_y)
    names(estimate) <- c("mean of x",
                         "mean of y")
    if (var.equal){
      df <- n_x + n_y - 2
      v <- 0
      if (n_x > 1)
        v <- v + (n_x - 1) * var_x
      if (n_y > 1)
        v <- v + (n_y - 1) * var_y
      v <- v/df
      stderr <- sqrt(v * (1/n_x + 1/n_y))
    }else{
      stderrx <- sqrt(var_x/n_x)
      stderry <- sqrt(var_y/n_y)
      stderr <- sqrt(stderrx^2 + stderry^2)
      df <- stderr^4/(stderrx^4/(n_x - 1) + stderry^4/(n_y - 1))
    }
    if(stderr < 10 *.Machine$double.eps * max(abs(mean_x), abs(mean_y)))
      stop("data are essentially constant")
    tstat <- (mean_x - mean_y - mu)/stderr
  }
  if(alternative == "less"){
    pval <- stats::pt(tstat, df)
    cint <- c(-Inf, tstat + stats::qt(conf.level, df))
  }
  else if(alternative == "greater"){
    pval <- stats::pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - stats::qt(conf.level, df), Inf)
  }else{
    pval <- 2 * stats::pt(-abs(tstat), df)
    alpha <- 1 - conf.level
    cint <- stats::qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if(!missing(mean_y))
    "difference in means"
  else "mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = pval,
               conf.int = cint, estimate = estimate, null.value = mu,
               stderr = stderr, alternative = alternative, method = method,
               data.name = dname)
  class(rval) <- "htest"
  return(rval)
}
