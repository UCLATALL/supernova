
<!-- README.md is generated from README.Rmd. Please edit that file -->

# supernova <img src="man/figures/logo.png" width="120" align="right" />

[![Travis build
status](https://travis-ci.org/UCLATALL/supernova.svg?branch=master)](https://travis-ci.org/UCLATALL/supernova)
[![Codecov test
coverage](https://codecov.io/gh/UCLATALL/supernova/branch/master/graph/badge.svg)](https://codecov.io/gh/UCLATALL/supernova?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/supernova)](https://cran.r-project.org/package=supernova)

The goal of `supernova` is to create ANOVA tables in the format used by
Judd, McClelland, and Ryan (2017, ISBN: 978-1138819832) in their
introductory textbook, *Data Analysis: A Model Comparison Approach to
Regression, ANOVA, and Beyond* [(book
website)](http://www.dataanalysisbook.com/index.html).\* These tables
include proportional reduction in error, a useful measure for teaching
the underlying concepts of ANOVA and regression, and formatting to ease
the transition between the book and R.

*\* Note: we are NOT affiliated with the authors or their institution.*

In keeping with the approach in Judd, McClelland, and Ryan (2017), the
ANOVA tables in this package are calculated using a model comparison
approach that should be understandable given a beginner’s understanding
of base R and the information from the book (so if you have these and
don’t understand what is going on in the code, let us know because we
are missing the mark\!). Here is an explanation of how the tables are
calculated for fully independent predictor varibles
(i.e. between-subjects designs):

1.  The “Total” row is calculated by updating the model passed to an
    empty model. For example, `lm(mpg ~ hp * disp, data = mtcars)` is
    updated to `(lm(mpg ~ NULL, data = mtcars))`. From this empty model,
    the sum of squares and df can be calculated.

2.  If there is at least one predictor in the model, the overall model
    row and the error row are calculated. In the vernacular of the book,
    the compact model is represented by the updated empty model from 1
    above, and the augmented model is the original model passed to
    `supernova()`. From these models the SSE(A) is calculated by
    `sum(resid(null_model) ^ 2)`, the SSR is calculated by SSE(C) -
    SSE(A), the PRE for the overall model is extracted from the fit
    (`r.squared`), the df for the error row is extracted from the `lm()`
    fit (`df.residuals`).

3.  If there are more than one predictors, the single term deletions are
    computed using `drop1()`. For the model `y ~ a * b` (which expands
    to `y ~ a + b + a:b`, where `a:b` is the interaction of `a` and
    `b`), `drop1()` essentially creates three models each with one term
    removed: `y ~ a + a:b`, `y ~ b + a:b`, and `y ~ a + b`. These models
    are considered the compact models which do not include the tested
    terms `a`, `b`, and `a:b`, respectively. `drop1()` computes the SSR
    (`Sum Sq`) and SSE(C) (`RSS`) for each of these augmented and
    compact model pairs, and these values are used to compute the SSR
    and PRE for each.

4.  Finally, the `MS` (`SS / df`), `F` (`MSR / MSE`), and `p` columns
    are calculated from already-computed values in the table.

In addition to the ANOVA table provided by `supernova()`, the
`supernova` package provides some useful functions for extracting
estimates from a linear model fit via `lm()`: `b0()`, `b1()`, `fVal()`,
`PRE()`. These are especially useful in the context of creating
bootstrapped sampling distributions as in the [Examples](#examples)
below.

### Supported models

The following models are explicitly tested and supported by
`supernova()`, *for independent samples (between-subjects) data only*.
For these models, there is also support for datasets with missing or
unbalanced data.

  - empty models: `y ~ NULL`
  - simple regression: `y ~ a`
  - multiple regression: `y ~ a + b`
  - interactive regression: `y ~ a * b`

Additionally, a subset of within-subjects designs are supported.
Importantly, only fully crossed (participants are observed in every
condition) and simply nested models (participants have multiple scores
in the same condition) have been tested. To accomodate these models
`supernova()` can accept models fit via `lmer()` as in the
[Examples](#examples) below.

Anything not included above is not (yet) explicitly tested and may yield
errors or incorrect statistics. This includes, but is not limited to

  - one-sample *t*-tests
  - mixed designs

## Installing

You can install the released version of supernova from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("supernova")
```

Alternatively you can download the package directly from this repository
using `devtools`:

``` r
library(devtools)
install_github("UCLATALL/supernova")
```

## Examples

Here are some basic examples of the code and output for this package:

### Between Subjects Models

#### A model with no predictors (null model)

``` r
supernova(lm(mpg ~ NULL, data = mtcars))
#>  Analysis of Variance Table (Type III SS)
#>  Model: mpg ~ NULL
#>  
#>                                SS  df     MS   F PRE   p
#>  ----- --------------- | -------- --- ------ --- --- ---
#>  Model (error reduced) |      --- ---    --- --- --- ---
#>  Error (from model)    |      --- ---    --- --- --- ---
#>  ----- --------------- | -------- --- ------ --- --- ---
#>  Total (empty model)   | 1126.047  31 36.324
```

#### A regression model with a single predictor

``` r
supernova(lm(mpg ~ hp, data = mtcars))
#>  Analysis of Variance Table (Type III SS)
#>  Model: mpg ~ hp
#>  
#>                                SS df      MS      F    PRE     p
#>  ----- --------------- | -------- -- ------- ------ ------ -----
#>  Model (error reduced) |  678.373  1 678.373 45.460 0.6024 .0000
#>  Error (from model)    |  447.674 30  14.922                    
#>  ----- --------------- | -------- -- ------- ------ ------ -----
#>  Total (empty model)   | 1126.047 31  36.324
```

#### A multiple regression model

``` r
supernova(lm(mpg ~ hp + disp, data = mtcars))
#>  Analysis of Variance Table (Type III SS)
#>  Model: mpg ~ hp + disp
#>  
#>                                SS df      MS      F    PRE     p
#>  ----- --------------- | -------- -- ------- ------ ------ -----
#>  Model (error reduced) |  842.554  2 421.277 43.095 0.7482 .0000
#>     hp                 |   33.665  1  33.665  3.444 0.1061 .0737
#>   disp                 |  164.181  1 164.181 16.795 0.3667 .0003
#>  Error (from model)    |  283.493 29   9.776                    
#>  ----- --------------- | -------- -- ------- ------ ------ -----
#>  Total (empty model)   | 1126.047 31  36.324
```

#### A multiple regression with an interaction

``` r
supernova(lm(mpg ~ hp * disp, data = mtcars))
#>  Analysis of Variance Table (Type III SS)
#>  Model: mpg ~ hp * disp
#>  
#>                                  SS df      MS      F    PRE     p
#>  ------- --------------- | -------- -- ------- ------ ------ -----
#>    Model (error reduced) |  923.189  3 307.730 42.475 0.8198 .0000
#>       hp                 |  113.393  1 113.393 15.651 0.3586 .0005
#>     disp                 |  188.449  1 188.449 26.011 0.4816 .0000
#>  hp:disp                 |   80.635  1  80.635 11.130 0.2844 .0024
#>    Error (from model)    |  202.858 28   7.245                    
#>  ------- --------------- | -------- -- ------- ------ ------ -----
#>    Total (empty model)   | 1126.047 31  36.324
```

#### Turn off the description column

``` r
supernova(lm(mpg ~ hp * disp, data = mtcars), verbose = FALSE)
#>  Analysis of Variance Table (Type III SS)
#>  Model: mpg ~ hp * disp
#>  
#>                  SS df      MS      F    PRE     p
#>  ------- | -------- -- ------- ------ ------ -----
#>  Model   |  923.189  3 307.730 42.475 0.8198 .0000
#>  hp      |  113.393  1 113.393 15.651 0.3586 .0005
#>  disp    |  188.449  1 188.449 26.011 0.4816 .0000
#>  hp:disp |   80.635  1  80.635 11.130 0.2844 .0024
#>  Error   |  202.858 28   7.245                    
#>  ------- | -------- -- ------- ------ ------ -----
#>  Total   | 1126.047 31  36.324
```

### Within Subjects Models

First let’s load up `lme4` which gives us `lmer()`, the function we will
use to fit within-subjects models. Additionally, install and load the
`JMRData` package which has some short datasets with non-independent
observations, and load `dplyr` and `tidyr` so that we can tidy the data.

``` r
# Run this line if you do not have the JMRData package
# remotes::install_github("UCLATALL/JMRData")

library(lme4)
library(tidyr)
library(dplyr)

simple_crossed <- JMRData::ex11.9 %>%
  tidyr::gather(condition, puzzles_completed, -subject) %>%
  dplyr::mutate_at(vars(subject, condition), as.factor) 

multiple_crossed <- JMRData::ex11.17 %>%
  tidyr::gather(condition, recall, -Subject) %>%
  tidyr::separate(condition, c("type", "time"), -1) %>%
  dplyr::mutate_at(vars(Subject, type, time), as.factor)
```

#### A one-way within-subjects design (crossed)

Fitting the `simple_crossed` data with `lm()` would ignore the
non-independence due to observations coming from the same `subject`.
Compare this output with the following output where the model was fit
with `lmer()` and the specification of `subject` as a random factor:

``` r
simple_crossed %>% 
  lm(puzzles_completed ~ condition, data = .) %>% 
  supernova(verbose = FALSE)
#>  Analysis of Variance Table (Type III SS)
#>  Model: puzzles_completed ~ condition
#>  
#>              SS df    MS     F    PRE     p
#>  ----- | ------ -- ----- ----- ------ -----
#>  Model |  2.250  1 2.250 1.518 0.0978 .2382
#>  Error | 20.750 14 1.482                   
#>  ----- | ------ -- ----- ----- ------ -----
#>  Total | 23.000 15 1.533

# use lmer() to specify the non-independence
simple_crossed %>% 
  lmer(puzzles_completed ~ condition + (1|subject), data = .) %>% 
  supernova()
#>  Analysis of Variance Table (Type III SS)
#>  Model: puzzles_completed ~ condition + (1 | subject)
#>  
#>                         SS df    MS     F    PRE     p
#>  ---------------- | ------ -- ----- ----- ------ -----
#>  Between Subjects |                                   
#>    Total          | 18.000  7 2.571                   
#>  ---------------- | ------ -- ----- ----- ------ -----
#>  Within Subjects  |                                   
#>    condition      |  2.250  1 2.250 5.727 0.4500 .0479
#>      Error        |  2.750  7 0.393                   
#>    Total          |  5.000  8 0.625                   
#>  ---------------- | ------ -- ----- ----- ------ -----
#>  Total            | 23.000 15 1.533
```

#### A two-way within-subjects design (crossed)

Here is another example like the previous, but here multiple variables
(`time`, `type`) and their interaction have been specified:

``` r
# fitting this with lm would ignore the non-independence due to Subject
multiple_crossed %>% 
  lm(recall ~ type * time, data = .) %>% 
  supernova(verbose = FALSE)
#>  Analysis of Variance Table (Type III SS)
#>  Model: recall ~ type * time
#>  
#>                   SS df     MS     F    PRE     p
#>  --------- | ------- -- ------ ----- ------ -----
#>  Model     |  85.367  5 17.073 2.791 0.3677 .0400
#>  type      |  12.100  1 12.100 1.978 0.0761 .1724
#>  time      |  44.400  2 22.200 3.629 0.2322 .0420
#>  type:time |   1.867  2  0.933 0.153 0.0126 .8593
#>  Error     | 146.800 24  6.117                   
#>  --------- | ------- -- ------ ----- ------ -----
#>  Total     | 232.167 29  8.006

# using lmer() we can specify the non-independence
multiple_crossed %>% 
  lmer(recall ~ type * time + (1|Subject) + (1|type:Subject) + (1|time:Subject), data = .) %>% 
  supernova()
#>  Analysis of Variance Table (Type III SS)
#>  Model: recall ~ type * time + (1 | Subject) + (1 | type:Subject) + (1 | time:Subject)
#>  
#>                          SS df     MS      F    PRE     p
#>  ---------------- | ------- -- ------ ------ ------ -----
#>  Between Subjects |                                      
#>    Total          | 131.001  4 32.750                    
#>  ---------------- | ------- -- ------ ------ ------ -----
#>  Within Subjects  |                                      
#>    type           |  17.633  1 17.633 11.377 0.7399 .0280
#>      Error        |   6.200  4  1.550                    
#>    time           |  65.867  2 32.933 29.940 0.8821 .0002
#>      Error        |   8.800  8  1.100                    
#>    type:time      |   1.867  2  0.933  9.333 0.7000 .0081
#>      Error        |   0.800  8  0.100                    
#>    Total          | 101.166 25  4.047                    
#>  ---------------- | ------- -- ------ ------ ------ -----
#>  Total            | 232.167 29  8.006
```

#### A one-way between-groups design (nested)

In this example, each person in a group of three generates a rating. If
we fit the data with `lm()` that would ignore the non-independence due
to the people being in the same `group`. Compare this output with the
foloowing output where the `group` is specified as a random factor.

``` r
simple_nested <- JMRData::ex11.1 %>%
  tidyr::gather(id, value, starts_with("score")) %>%
  dplyr::mutate_at(vars(group, instructions, id), as.factor)

# fitting this with lm would ignore the non-independence due to group
simple_nested %>% 
  lm(value ~ instructions, data = .) %>% 
  supernova(verbose = FALSE)
#>  Analysis of Variance Table (Type III SS)
#>  Model: value ~ instructions
#>  
#>              SS df     MS      F    PRE     p
#>  ----- | ------ -- ------ ------ ------ -----
#>  Model | 12.500  1 12.500 12.500 0.4386 .0027
#>  Error | 16.000 16  1.000                    
#>  ----- | ------ -- ------ ------ ------ -----
#>  Total | 28.500 17  1.676

# using lmer() we can specify the non-independence
simple_nested %>% 
  lmer(value ~ instructions + (1|group), data = .) %>% 
  supernova()
#>  Analysis of Variance Table (Type III SS)
#>  Model: value ~ instructions + (1 | group)
#>  
#>                         SS df     MS     F    PRE     p
#>  ---------------- | ------ -- ------ ----- ------ -----
#>  Between Subjects |                                    
#>    instructions   | 12.500  1 12.500 4.686 0.5395 .0964
#>      Error        | 10.671  4  2.668                   
#>    Total          | 23.171  5  4.634                   
#>  ---------------- | ------ -- ------ ----- ------ -----
#>  Within Subjects  |                                    
#>    Total          |  5.329 12  0.444                   
#>  ---------------- | ------ -- ------ ----- ------ -----
#>  Total            | 28.500 17  1.676
```

### Using Different SS Types

#### Type III: Orthogonal (default)

``` r
supernova(lm(mpg ~ hp * disp, data = mtcars))
#>  Analysis of Variance Table (Type III SS)
#>  Model: mpg ~ hp * disp
#>  
#>                                  SS df      MS      F    PRE     p
#>  ------- --------------- | -------- -- ------- ------ ------ -----
#>    Model (error reduced) |  923.189  3 307.730 42.475 0.8198 .0000
#>       hp                 |  113.393  1 113.393 15.651 0.3586 .0005
#>     disp                 |  188.449  1 188.449 26.011 0.4816 .0000
#>  hp:disp                 |   80.635  1  80.635 11.130 0.2844 .0024
#>    Error (from model)    |  202.858 28   7.245                    
#>  ------- --------------- | -------- -- ------- ------ ------ -----
#>    Total (empty model)   | 1126.047 31  36.324
```

These are equivalent to the above:

``` r
supernova(lm(mpg ~ hp * disp, data = mtcars), type = 3)
supernova(lm(mpg ~ hp * disp, data = mtcars), type = "III")
supernova(lm(mpg ~ hp * disp, data = mtcars), type = "orthogonal")
```

#### Type I: Sequential

``` r
supernova(lm(mpg ~ hp * disp, data = mtcars), type = 1)
#>  Analysis of Variance Table (Type I SS)
#>  Model: mpg ~ hp * disp
#>  
#>                                  SS df      MS      F    PRE     p
#>  ------- --------------- | -------- -- ------- ------ ------ -----
#>    Model (error reduced) |  923.189  3 307.730 42.475 0.8198 .0000
#>       hp                 |  678.373  1 678.373 93.634 0.7698 .0000
#>     disp                 |  164.181  1 164.181 22.661 0.4473 .0001
#>  hp:disp                 |   80.635  1  80.635 11.130 0.2844 .0024
#>    Error (from model)    |  202.858 28   7.245                    
#>  ------- --------------- | -------- -- ------- ------ ------ -----
#>    Total (empty model)   | 1126.047 31  36.324
```

These are equivalent to the above:

``` r
supernova(lm(mpg ~ hp * disp, data = mtcars), type = "I")
supernova(lm(mpg ~ hp * disp, data = mtcars), type = "sequential")
```

#### Type II: Hierarchical

``` r
supernova(lm(mpg ~ hp * disp, data = mtcars), type = 2)
#>  Analysis of Variance Table (Type II SS)
#>  Model: mpg ~ hp * disp
#>  
#>                                  SS df      MS      F    PRE     p
#>  ------- --------------- | -------- -- ------- ------ ------ -----
#>    Model (error reduced) |  923.189  3 307.730 42.475 0.8198 .0000
#>       hp                 |   33.665  1  33.665  4.647 0.1423 .0399
#>     disp                 |  164.181  1 164.181 22.661 0.4473 .0001
#>  hp:disp                 |   80.635  1  80.635 11.130 0.2844 .0024
#>    Error (from model)    |  202.858 28   7.245                    
#>  ------- --------------- | -------- -- ------- ------ ------ -----
#>    Total (empty model)   | 1126.047 31  36.324
```

These are equivalent to the above:

``` r
supernova(lm(mpg ~ hp * disp, data = mtcars), type = "II")
supernova(lm(mpg ~ hp * disp, data = mtcars), type = "hierarchical")
```

### Displaying which models were compared

This package is based on a model comparison approach to understanding
regression and ANOVA. As such it is useful to know which models are
being compared for any given term in an ANOVA table. The
`generate_models()` function accepts a linear model and the desired type
of SS and returns a list of the models that should be compared to
appropriately evaluate each term in the full model.

``` r
generate_models(lm(mpg ~ hp * disp, data = mtcars), type = 2)
#> Comparison Models for Type II SS
#> Model: mpg ~ hp * disp
#> 
#> Full Model
#>   complex: mpg ~ hp + disp + hp:disp
#>    simple: mpg ~ NULL
#> hp
#>   complex: mpg ~ hp + disp
#>    simple: mpg ~      disp
#> disp
#>   complex: mpg ~ hp + disp
#>    simple: mpg ~ hp
#> hp:disp
#>   complex: mpg ~ hp + disp + hp:disp
#>    simple: mpg ~ hp + disp
```

### Bootstrapping Estimates

The estimate extraction functions in the package simplify the ability to
create bootstrapped sampling distributions of those estimates. The
functions currently exported are `PRE`, `b0`, `b1`, `fVal`, `SSM`/`SSR`,
and `SSE`. Other terms can be bootstrapped as well, the target estimated
just needs to be extracted via other means.

#### Bootstrapping the slope of a simple model

``` r
# to extract a single estimate:
b1(lm(mpg ~ hp, data = mtcars))
#> [1] -0.06823

# use mosaic package to repetetively resample to bootstrap a distribution
sd_of_b1 <- mosaic::do(1000) * b1(lm(mpg ~ hp, data = mosaic::resample(mtcars)))

# plot the bootstrapped estimates
hist(sd_of_b1$b1)
```

<img src="man/figures/README-samp_dist_of_b1-1.png" width="100%" />

#### Bootstrapping the effect of one term from a multiple regression

``` r
sd_of_hp <- mosaic::do(1000) * {
  # create a new model from the resampled data
  model <- lm(mpg ~ disp * hp, data = mosaic::resample(mtcars))
  
  # extract the desired estimate, here the coefficient for hp
  coef(model)[["hp"]]
}

# plot the bootstrapped estimates
hist(sd_of_hp$result)
```

<img src="man/figures/README-samp_dist_of_hp_coef-1.png" width="100%" />

# Contributing

If you see an issue, problem, or improvement that you think we should
know about, or you think would fit with this package, please let us know
on our [issues page](https://github.com/UCLATALL/supernova/issues).
Alternatively, if you are up for a little coding of your own, submit a
pull request:

1.  Fork it\!
2.  Create your feature branch: `git checkout -b my-new-feature`
3.  Commit your changes: `git commit -am 'Add some feature'`
4.  Push to the branch: `git push origin my-new-feature`
5.  Submit a [pull request](https://github.com/UCLATALL/supernova/pulls)
    :D
