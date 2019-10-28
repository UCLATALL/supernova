context("supernova: Printing")

test_that("supernova tables object has explanatory header for lm", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  printed <- capture.output(supernova(model, type = 1))
  expect_identical(printed[1:3], c(
    " Analysis of Variance Table (Type I SS)",
    " Model: mpg ~ hp * disp",
    " "
  ))

  printed <- capture.output(supernova(model, type = 2))
  expect_identical(printed[[1]], " Analysis of Variance Table (Type II SS)")

  printed <- capture.output(supernova(model, type = 3))
  expect_identical(printed[[1]], " Analysis of Variance Table (Type III SS)")
})

test_that("columns have general ANOVA table names", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  printed <- capture.output(supernova(model, type = 1))
  expect_match(printed[[4]], "^\\s+SS\\s+df\\s+MS\\s+F\\s+PRE\\s+p$")
})

test_that("null model tables are beautifully formatted", {
  model <- lm(mpg ~ NULL, data = mtcars)
  printed <- capture.output(supernova(model))
  expect_identical(printed[4:9], c(
    "                               SS  df     MS   F PRE   p",
    " ----- ----------------- -------- --- ------ --- --- ---",
    " Model (error reduced) |      --- ---    --- --- --- ---",
    " Error (from model)    |      --- ---    --- --- --- ---",
    " ----- ----------------- -------- --- ------ --- --- ---",
    " Total (empty model)   | 1126.047  31 36.324            "
  ))
})

test_that("single predictor tables are beautifully formatted", {
  model <- lm(mpg ~ hp, data = mtcars)
  printed <- capture.output(supernova(model))
  expect_identical(printed[4:9], c(
    "                               SS df      MS      F    PRE     p",
    " ----- ----------------- -------- -- ------- ------ ------ -----",
    " Model (error reduced) |  678.373  1 678.373 45.460 0.6024 .0000",
    " Error (from model)    |  447.674 30  14.922                    ",
    " ----- ----------------- -------- -- ------- ------ ------ -----",
    " Total (empty model)   | 1126.047 31  36.324                    "
  ))
})

test_that("multiple predictor tables are beautifully formatted", {
  model <- lm(mpg ~ hp + disp, data = mtcars)
  printed <- capture.output(supernova(model))
  expect_identical(printed[4:11], c(
    "                               SS df      MS      F    PRE     p",
    " ----- ----------------- -------- -- ------- ------ ------ -----",
    " Model (error reduced) |  842.554  2 421.277 43.095 0.7482 .0000",
    "    hp                 |   33.665  1  33.665  3.444 0.1061 .0737",
    "  disp                 |  164.181  1 164.181 16.795 0.3667 .0003",
    " Error (from model)    |  283.493 29   9.776                    ",
    " ----- ----------------- -------- -- ------- ------ ------ -----",
    " Total (empty model)   | 1126.047 31  36.324                    "
  ))
})

test_that("non-verbose tables do not have a description column", {
  model <- lm(mpg ~ NULL, data = mtcars)
  printed <- capture.output(supernova(model, verbose = FALSE))
  expect_identical(printed[4:9], c(
    "               SS  df     MS   F PRE   p",
    " ------- -------- --- ------ --- --- ---",
    " Model |      --- ---    --- --- --- ---",
    " Error |      --- ---    --- --- --- ---",
    " ------- -------- --- ------ --- --- ---",
    " Total | 1126.047  31 36.324            "
  ))
})

test_that("repeated measures tables are beautifully formatted", {
  # jmr_ex11.17 <- readRDS("./tests/testthat/cache/jmr_ex11.17.Rds")
  jmr_ex11.17 <- readRDS("./cache/jmr_ex11.17.Rds")
  model <- lme4::lmer(
    recall ~ time * type + (1|subject) + (1|time:subject) + (1|type:subject),
    data = jmr_ex11.17
  )
  printed <- capture.output(supernova(model))
  expect_identical(printed[1:18], c(
    " Analysis of Variance Table (Type III SS)",
    " Model: recall ~ time * type + (1 | subject) + (1 | time:subject) + (1 | type:subject)",
    " ",
    "                         SS df     MS      F    PRE     p",
    " ------------------ ------- -- ------ ------ ------ -----",
    " Between Subjects |                                      ",
    "   Total          | 131.001  4 32.750                    ",
    " ------------------ ------- -- ------ ------ ------ -----",
    " Within Subjects  |                                      ",
    "   time           |  65.867  2 32.933 29.940 0.8821 .0002",
    "     Error        |   8.800  8  1.100                    ",
    "   type           |  17.633  1 17.633 11.377 0.7399 .0280",
    "     Error        |   6.200  4  1.550                    ",
    "   time:type      |   1.867  2  0.933  9.333 0.7000 .0081",
    "     Error        |   0.800  8  0.100                    ",
    "   Total          | 101.166 25  4.047                    ",
    " ------------------ ------- -- ------ ------ ------ -----",
    " Total            | 232.167 29  8.006                    "
  ))
})
