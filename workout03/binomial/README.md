
<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
--------

`"binomial"` is an [R](http://www.r-project.org/) package that provides functions for binomial distribution summaries

-   `bin_choose()` chooses k from n
-   `bin_probability()` computes probability of k success in n trials
-   `bin_distribution()` creates object of class "bindis"
-   `bin_cumulative()` creates object of class "bincum"
-   `bin_variable()` creates object of class "binvar"
-   `bin_mean()` computes mean of a binomial distribution
-   `bin_variance()` computes variance of a binomial distribution
-   `bin_mode()` computes mode of a binomial distribution
-   `bin_skewness()` computes skewness of a binomial distribution
-   `bin_kurtosis()` computes kurtosis of a binomial distribution

Motivation
----------

This package has been developed for workout03 of the spring 2019 semester of Stat 133

Usage
-----

``` r
library(binomial)
# creating a binomial variable
bin1 <- bin_variable(trials=5, prob=0.3)
bin1
#> "Binomial variable"
#> 
#> Parameters
#> - number of trials: 5
#> - probability of success : 0.3

# cumulative distribution
bin2 <- bin_cumulative(trials=5, prob=0.3)
bin2
#>   success probability cumulative
#> 1       0     0.16807    0.16807
#> 2       1     0.36015    0.52822
#> 3       2     0.30870    0.83692
#> 4       3     0.13230    0.96922
#> 5       4     0.02835    0.99757
#> 6       5     0.00243    1.00000

# summary
summary(bin1)
#> "Summary Binomial"
#> 
#> Parameters
#> - number of trials: 5
#> - prob of success : 0.3
#> 
#> Measures
#> - mean    : 1.5
#> - variance: 1.05
#> - mode    : 1
#> - skewness: 0.39036
#> - kurtosis: -0.247619
```
