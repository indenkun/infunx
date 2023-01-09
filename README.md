
<!-- README.md is generated from README.Rmd. Please edit that file -->

# infunx

<!-- badges: start -->
<!-- badges: end -->

This is a collection of R utilities functions for me, but maybe also for
you.

Functions may be added, specifications of functions may change or become
obsolete, and names may change without notice.

This is a collection of codes that cannot be included in `{infun}` due
to licensing reasons.

## Installation

You can install the development version of `{infunx}` like so:

``` r
install.packages("remotes")
remotes::install_github("indenkun/infunx")
```

## Example

load library.

``` r
library(infunx)
```

### `t_test_stats()`

Performs one and two sample t-tests from descriptive statistics. In
short, it is like `python`’s `scipy.stats.ttest_ind_from_stats`.

This is partially based on the code of `t.test()` in `{stats}` on R
4.2.2 by R Core Team.

``` r
# The mean value of a is 2, with a standard deviation of 1 and a sample size of 3.
# The mean value of b is 0, with a standard deviation of 1 and a sample size of 3.
a <- c(1, 2, 3)
b <- c(-1, 0, 1)
# Using t.test() of stats for this 2-group t-test, we get the following.
stats::t.test(a, b)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  a and b
#> t = 2.4495, df = 4, p-value = 0.07048
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.2669579  4.2669579
#> sample estimates:
#> mean of x mean of y 
#>         2         0
```

This t-test can be performed on aggregated descriptive statistics data
with `t_test_stats()`.

``` r
t_test_stats(mean_x = 2, std_x = 1, n_x = 3,
             mean_y = 0, std_y = 1, n_y = 3)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  mean of 2 and mean of 0
#> t = 2.4495, df = 4, p-value = 0.07048
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.2669579  4.2669579
#> sample estimates:
#> mean of x mean of y 
#>         2         0
```

One sample t-tests can also be performed.

``` r
stats::t.test(a)
#> 
#>  One Sample t-test
#> 
#> data:  a
#> t = 3.4641, df = 2, p-value = 0.07418
#> alternative hypothesis: true mean is not equal to 0
#> 95 percent confidence interval:
#>  -0.4841377  4.4841377
#> sample estimates:
#> mean of x 
#>         2
t_test_stats(mean_x = 2, std_x = 1, n_x = 3)
#> 
#>  One Sample t-test
#> 
#> data:  mean of 2
#> t = 3.4641, df = 2, p-value = 0.07418
#> alternative hypothesis: true mean is not equal to 0
#> 95 percent confidence interval:
#>  -0.4841377  4.4841377
#> sample estimates:
#> mean of x 
#>         2
```

## License

- GPL-2.0 license

## Imports packages

- `{stats}`

## Notice

- The email address listed in the DESCRIPTION is a dummy. If you have
  any questions, please post them on ISSUE.
