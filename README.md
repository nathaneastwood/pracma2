<a name="readme-top"></a>

# pracma2

[![CRAN status](https://www.r-pkg.org/badges/version/pracma2)]()

## Overview

This is a very simple package which offers a reimplementation of
`pracma::findpeaks()`. `pracma::findpeaks()` uses character strings and
regex to find the peaks whereas `pracma2::findpeaks()` uses integers,
which is faster.

<p align="right">
(<a href="#readme-top">back to top</a>)
</p>

## Installation

You can install the package with

``` r
# install.packages("remotes")
remotes::install_github("nathaneastwood/pracma2")
```

<p align="right">
(<a href="#readme-top">back to top</a>)
</p>

## Usage

``` r
library(pracma2)

x <- c(2, 12, 4, 6, 9, 4, 3, 1, 19, 7)

findpeaks(x)
#      [,1] [,2] [,3] [,4]
# [1,]   12    2    1    3
# [2,]    9    5    3    8
# [3,]   19    9    8   10

findpeaks(x, minpeakheight = 15)
#      [,1] [,2] [,3] [,4]
# [1,]   19    9    8   10
```

<p align="right">
(<a href="#readme-top">back to top</a>)
</p>

## Related Work

-   [pracma](https://cran.r-project.org/web/packages/pracma/index.html)

<p align="right">
(<a href="#readme-top">back to top</a>)
</p>
