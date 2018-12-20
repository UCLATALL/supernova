# supernova

The goal of `supernova` is to create ANOVA tables in the format used by Judd, 
McClelland, and Ryan (2017, ISBN:978-1138819832) in their introductory textbook, 
*Data Analysis: A Model Comparison Approach to Regression, ANOVA, and Beyond* 
[(book website)](http://www.dataanalysisbook.com/index.html). 
These tables include proportional reduction in error, a useful measure for 
teaching the underlying concepts of ANOVA and regression, and formatting to ease 
the transition between the book and R.

Additionally, `supernova` provides some useful functions for extracting estimates
from a linear model: `b0()`, `b1()`, `fVal()`, `PRE()`. These are especially
useful in the context of creating bootstrapped sampling distributions as in the
examples below.

Note: we are NOT affiliated with the authors or their institution.

## Installing

You can install the released version of supernova from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("supernova")
```

Alternatively you can download the package directly from this repository using
`devtools`:

``` r
library(devtools)
install_github("UCLATALL/supernova")
```

## Examples

Here are some basic examples of the code and output for this package:

### Testing a model with no predictors (null model)
``` r
supernova(lm(mpg ~ NULL, data = mtcars))

#> Analysis of Variance Table
#> Outcome variable: mpg 
#> Model: mpg ~ NULL
#> 
#>                               SS  df     MS   F PRE   p
#> ----- ----------------- -------- --- ------ --- --- ---
#> Model (error reduced) |      --- ---    --- --- --- ---
#> Error (from model)    |      --- ---    --- --- --- ---
#> ----- ----------------- -------- --- ------ --- --- ---
#> Total (empty model)   | 1126.047  31 36.324            
```

### Testing a regression model with a single predictor
``` r
supernova(lm(mpg ~ hp, data = mtcars))

#> Analysis of Variance Table
#> Outcome variable: mpg 
#> Model: mpg ~ hp
#> 
#>                               SS df      MS      F    PRE     p
#> ----- ----------------- -------- -- ------- ------ ------ -----
#> Model (error reduced) |  678.373  1 678.373 45.460 0.6024 .0000
#> Error (from model)    |  447.674 30  14.922                    
#> ----- ----------------- -------- -- ------- ------ ------ -----
#> Total (empty model)   | 1126.047 31  36.324          
```

### Multiple regression
``` r
supernova(lm(mpg ~ hp + disp, data = mtcars))

#> Analysis of Variance Table
#> Outcome variable: mpg 
#> Model: mpg ~ hp + disp
#> 
#>                               SS df      MS      F    PRE     p
#> ----- ----------------- -------- -- ------- ------ ------ -----
#> Model (error reduced) |  842.554  2 421.277 43.095 0.7482 .0000
#>    hp                 |   33.665  1  33.665  3.444 0.1061 .0737
#>  disp                 |  164.181  1 164.181 16.795 0.3667 .0003
#> Error (from model)    |  283.493 29   9.776                    
#> ----- ----------------- -------- -- ------- ------ ------ -----
#> Total (empty model)   | 1126.047 31  36.324                    
```

### Multiple regression with interaction (Type III SS)
``` r
supernova(lm(mpg ~ hp * disp, data = mtcars))

#> Analysis of Variance Table
#> Outcome variable: mpg 
#> Model: mpg ~ hp * disp
#> 
#>                                 SS df      MS      F    PRE     p
#> ------- ----------------- -------- -- ------- ------ ------ -----
#>   Model (error reduced) |  923.189  3 307.730 42.475 0.8198 .0000
#>      hp                 |  113.393  1 113.393 15.651 0.3586 .0005
#>    disp                 |  188.449  1 188.449 26.011 0.4816 .0000
#> hp:disp                 |   80.635  1  80.635 11.130 0.2844 .0024
#>   Error (from model)    |  202.858 28   7.245                    
#> ------- ----------------- -------- -- ------- ------ ------ -----
#>   Total (empty model)   | 1126.047 31  36.324                                       
```

### Bootstrapping a sampling distribution of the slope
``` r
# extract a single estimate:
b1(lm(mpg ~ hp, data = mtcars))

# use mosaic package for repetition and resampling to bootstrap a distribution
sd_of_b1 <- mosaic::do(1000) * b1(lm(mpg ~ hp, data = mosaic::resample(mtcars, 30)))
hist(sd_of_b1$bi)
```

# Contributing

If you see an issue, problem, or improvement that you think we should know about, 
or you think would fit with this package, please let us know on our 
[issues page](https://github.com/UCLATALL/supernova/issues). Alternatively, if
you are up for a little coding of your own, submit a pull request:

1. Fork it!
2. Create your feature branch: ```git checkout -b my-new-feature```
3. Commit your changes: ```git commit -am 'Add some feature'```
4. Push to the branch: ```git push origin my-new-feature```
5. Submit a [pull request](https://github.com/UCLATALL/supernova/pulls) :D
