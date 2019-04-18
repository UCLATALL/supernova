context("test-reexports")

test_data <- data.frame(x = 1:4, y = 4:7, z = rep(c("a", "b"), times = 2))

test_that("re-exported mosaic formula-aware functions work properly", {
  x_quantiles <- stats::quantile(test_data$x)

  cor(test_data$x, test_data$y) %>%
    expect_equal(stats::cor(test_data$x, test_data$y)) %>%
    expect_identical(cor(x ~ y, data = test_data))

  cov(test_data$x, test_data$y) %>%
    expect_equal(stats::cov(test_data$x, test_data$y)) %>%
    expect_identical(cov(x ~ y, data = test_data))

  favstats(test_data$x) %>%
    expect_equal(data.frame(
      min = base::min(test_data$x),
      Q1 = x_quantiles[[2]],
      median = x_quantiles[[3]],
      Q3 = x_quantiles[[4]],
      max = base::max(test_data$x),
      mean = base::mean(test_data$x),
      sd = stats::sd(test_data$x),
      n = base::length(test_data$x),
      missing = 0L,
      row.names = ""
    )) %>%
    expect_identical(favstats(~x, data = test_data))

  IQR(test_data$x) %>%
    expect_equal(x_quantiles[[4]] - x_quantiles[[2]]) %>%
    expect_identical(IQR(~x, data = test_data))

  mean(test_data$x) %>%
    expect_equal(base::mean(test_data$x)) %>%
    expect_identical(mean(~x, data = test_data))

  median(test_data$x) %>%
    expect_equal(x_quantiles[[3]]) %>%
    expect_identical(median(~x, data = test_data))

  max(test_data$x) %>%
    expect_equal(base::max(test_data$x)) %>%
    expect_identical(max(~x, data = test_data))

  min(test_data$x) %>%
    expect_equal(base::min(test_data$x)) %>%
    expect_identical(min(~x, data = test_data))

  prod(test_data$x) %>%
    expect_equal(base::prod(test_data$x)) %>%
    expect_identical(prod(~x, data = test_data))

  range(test_data$x) %>%
    expect_equal(base::range(test_data$x)) %>%
    expect_identical(range(~x, data = test_data))

  sd(test_data$x) %>%
    expect_equal(stats::sd(test_data$x)) %>%
    expect_identical(sd(~x, data = test_data))

  sum(test_data$x) %>%
    expect_equal(base::sum(test_data$x)) %>%
    expect_identical(sum(~x, data = test_data))

  var(test_data$x) %>%
    expect_equal(stats::var(test_data$x)) %>%
    expect_identical(var(~x, data = test_data))
})

test_that("all other re-exports do not throw errors for normal use", {
  # lsr
  expect_error(cohensD(x ~ z, data = test_data), NA)

  # dplyr
  expect_error(arrange(test_data, x), NA)
  expect_error(filter(test_data, x == 1), NA)
  expect_error(mutate(test_data, z = x + 1), NA)
  expect_error(recode(1:3, "a", "b", "c"), NA)
  expect_error(select(test_data, x), NA)

  # magrittr
  expect_error(1:3 %>% mean(), NA)

  # remaining mosaic
  expect_error(do(1) * resample(test_data$x, 1), NA)
  expect_error(resample(test_data$x, 1), NA)
  expect_error(sample(test_data$x, 1), NA)
  expect_error(shuffle(test_data$x), NA)
  expect_error(xcnorm(.95), NA)
  expect_error(xpnorm(1.96), NA)
  expect_error(xqnorm(.975), NA)
  expect_error(xcf(.95, 2, 32), NA)
  expect_error(xpf(1.96, 2, 32), NA)
  expect_error(xqf(.975, 2, 32), NA)
  expect_error(zscore(1:3), NA)

  # mosaicCore
  expect_error(count(~x, data = test_data), NA)
  expect_error(tally(~x, data = test_data), NA)
  expect_error(makeFun(lm(x ~ y, data = test_data)), NA)
  expect_error(fit_distr_fun(~x, data = test_data, dist = "dnorm"), NA)

  # MASS
  expect_error(fitdistr(test_data$x, "norm"))

  # ggformula
  expect_error(gf_bar(x ~ z, data = test_data), NA)
  expect_error(gf_boxplot(x ~ z, data = test_data), NA)
  expect_error(gf_boxploth(x ~ z, data = test_data), NA)
  expect_error(gf_density(~x, data = test_data), NA)
  expect_error(gf_dist("norm"), NA)
  expect_error(gf_fitdistr(~x, data = test_data), NA)
  expect_error(gf_histogram(~x, data = test_data), NA)
  expect_error(gf_dhistogram(~x, data = test_data), NA)
  expect_error(gf_histogramh(~x, data = test_data), NA)
  expect_error(gf_dhistogramh(~x, data = test_data), NA)
  expect_error(gf_point(x ~ z, data = test_data), NA)
  expect_error(gf_jitter(x ~ z, data = test_data), NA)
  expect_error(gf_lm(x ~ y, data = test_data), NA)
  expect_error(gf_abline(slope = 1, intercept = 1), NA)
  expect_error(gf_hline(yintercept = 1), NA)
  expect_error(gf_vline(xintercept = 1), NA)
  expect_error(gf_bar(x ~ z, data = test_data) %>% gf_facet_wrap(~y), NA)
  expect_error(gf_bar(x ~ z, data = test_data) %>% gf_facet_grid(. ~ y), NA)
  expect_error(gf_bar(x ~ z, data = test_data) %>% gf_labs(x = "Title"), NA)
})
