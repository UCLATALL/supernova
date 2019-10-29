# supernova 2.2.0

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
