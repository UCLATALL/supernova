# supernova 2.4.4

* Remove `supernova-vctrs` from exports

# supernova 2.4.3

* Fix issues with `lintr` causing `R CMD CHECK` to fail
* Change maintainer to [@adamblake](https://github.com/adamblake)
* Change mislabeled factor level in `Fingers$Interest` to "Very Interested"


# supernova 2.4.2

* Ensure appropriate version of `pillar` is available (thanks @cedricbatailler)


# supernova 2.4.1

* Small tweaks to make the package work on R 3.4.0


# supernova 2.4.0

## New features

There are four new pairwise comparisons functions:

- `pairwise()`
- `pairwise_t()`
- `pairwise_bonferroni()`
- `pairwise_tukey()`

Each of these determines all the pairwise comparisons that can be made for a model (fit by `lm()`) and then computes the comparisons. For `pairwise_t()` no correction is made for multiple comparisons, but for the others, the named correction is made. These corrections can also be specified as arguments to the `pairwise()` wrapper function. Each function produces output that has customized printing, supports most (if not all) normal data frame actions, and a plotting function that graphs the mean differences and their confidence intervals.


# supernova 2.3.0

* Dependency on `lme4` is moved to Suggests. Models implementing `lmerMod` are handled via `supernova.lmerMod` and `variables.lmerMod` but use of the `lme4` package is limited to tests.
* More robust and readable implementation of `variables()` using the new formula utility functions added. See `?formula_building`, `?formula_expansion`, and `?formula_extraction`.
* Add a new function `equation()` to extract the fitted equation from a linear model (`lm()`) (thanks for the suggestion from [@ave-63](https://github.com/ave-63)!)


# supernova 2.2.3

* Remove dependency on `dplyr` because it changes too quickly and has too many other dependencies
* Mild refactoring to improve code readability


# supernova 2.2.2

* Patch to keep up with changes to `lme4`


# supernova 2.2.1

* Add support for mixed models (as in nested and crossed). See the README for more information.


# supernova 2.2.0

Extend supernova to handle within (crossed) designs

* Add `lme4` and `dplyr` to Imports
* Update R dependency to 3.5.0 (for serialized data; Rds)
* Convert `supernova` to S3 class with methods for `lm` and `lmerMod`
* Add tests for `supernova()` for crossed (but not nested) `lmer()` fits
* Extend `print.supernova` to handle new models

Minor changes:

* Refactor utility functions into utils.R
* Add internal documentation for utility functions


# supernova 2.1.1

* Added a `NEWS.md` file to track changes to the package.

* Created and added a logo to the package. (#21, @adamblake)

* Added the ability to change the type of sums of squares to calculate when computing the ANOVA tables. Users can choose from 1/I/sequential, 2/II/hierarchical, 3/III/orthogonal. (#22, @adamblake)

* Added pedagogical function `generate_models()` for showing which models are being compared when evaluating terms in a model. This function also supports specification of the type of sums of squares to use.  (#22, @adamblake)

* Updated the README to be generated from an Rmd file and to include information and examples regarding how to calculate different SS types and how to use `generate_models()`

* Added a data frame identical to Servers named Tables. This is a more appropriate name for the dataset because each row describes what happened at a table in the restaurant.
  - Updated variable names and documentation to "table" as well.
  - Added deprecation notice to Servers documentation as the table will be removed in the future.


# supernova 2.0.0

* Added support for multiple regression using Type III sums of squares

* Updated README for more information, examples, and a description of how the calculation of the ANOVA tables follows the model comparison approach used in Judd, McClelland, & Ryan (2017).

# supernova 1.1.0

This version of supernova is the original distributed on CRAN. Calculation of supernova() tables with *multiple* predictor variables in this version will not produce output similar to the reference text, Judd, McClelland, and Ryan. However, the values for *single* predictor models are correct.
