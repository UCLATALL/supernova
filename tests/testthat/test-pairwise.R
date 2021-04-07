test_that("not specifying which term to break down will break down all categorical terms", {
  fit <- lm(Thumb ~ Sex * RaceEthnic, supernova::Fingers)
  cat_terms <- frm_terms(fit)
  expect_named(pairwise(fit, "Tukey"), cat_terms)
  expect_named(pairwise(fit, "none"), cat_terms)
})

test_that("you can specify the specific term you want to analyze", {
  fit <- lm(Thumb ~ Sex * RaceEthnic, supernova::Fingers)
  expect_named(pairwise(fit, term = "Sex:RaceEthnic"), "Sex:RaceEthnic")
})

test_that("there is an informative error if you try to select a non-existent term", {
  fit <- lm(Thumb ~ Sex * RaceEthnic, supernova::Fingers)
  expect_snapshot_error(pairwise(fit, "does-not-exist"))
})

test_that("pairwise wraps pairwise_tukey", {
  fit <- lm(Thumb ~ RaceEthnic, supernova::Fingers)
  expect_equal(pairwise(fit, "Tukey"), pairwise_tukey(fit))
})

test_that("pairwise_tukey outputs correct values for diff, lower, upper, and p", {
  fit <- lm(Thumb ~ RaceEthnic * Sex, supernova::Fingers)

  actual <- pairwise_tukey(fit)[[1]][, c("diff", "lower", "upper", "p_adj")]
  actual <- purrr::modify(actual, as.double)
  expected <- as.data.frame(TukeyHSD(aov(fit))[[1]])
  names(expected) <- names(actual)

  ignorable <- c("row.names", "class", "fit", "term", "correction", "n_levels", "alpha", "fwer")
  expect_equal(actual, expected, ignore_attr = ignorable)
})

test_that("pairwise_t outputs correct p values for t tests with pooled sd", {
  fit <- lm(Thumb ~ RaceEthnic, supernova::Fingers)
  expected <- pairwise.t.test(
    Fingers$Thumb,
    Fingers$RaceEthnic,
    p.adjust.method = "none",
    pool.sd = TRUE
  ) %>%
    magrittr::extract2("p.value") %>%
    lower_tri(diag = TRUE)

  actual <- pairwise_t(fit)$RaceEthnic$p_val %>%
    as.double()

  expect_equal(actual, expected)
})

test_that("pairwise_bonferroni outputs correct corrected p values for t tests with pooled sd", {
  fit <- lm(Thumb ~ RaceEthnic, supernova::Fingers)
  expected <- pairwise.t.test(
    Fingers$Thumb,
    Fingers$RaceEthnic,
    p.adjust.method = "bonferroni",
    pool.sd = TRUE
  ) %>%
    magrittr::extract2("p.value") %>%
    lower_tri(diag = TRUE)

  actual <- pairwise_bonferroni(fit)$RaceEthnic$p_adj %>%
    as.double()

  expect_equal(actual, expected)
})

test_that("each type of comparisons object plots well", {
  fit <- lm(Thumb ~ RaceEthnic, supernova::Fingers)

  tukey <- ggplot2::autoplot(pairwise_tukey(fit))
  vdiffr::expect_doppelganger("Tukey, one variable", tukey$RaceEthnic)

  t <- ggplot2::autoplot(pairwise_t(fit))
  vdiffr::expect_doppelganger("t-test, one variable", t$RaceEthnic)

  bonf <- ggplot2::autoplot(pairwise_bonferroni(fit))
  vdiffr::expect_doppelganger("Bonferroni, one variable", bonf$RaceEthnic)
})

test_that("a separate plot is created for each term in the model", {
  fit <- lm(Thumb ~ Sex * RaceEthnic, supernova::Fingers)
  plots <- ggplot2::autoplot(pairwise_tukey(fit))
  vdiffr::expect_doppelganger("Multiple plots, Sex", plots$Sex)
  vdiffr::expect_doppelganger("Multiple plots, RaceEthnic", plots$RaceEthnic)
  vdiffr::expect_doppelganger("Multiple plots, Sex:RaceEthnic", plots$`Sex:RaceEthnic`)
})
