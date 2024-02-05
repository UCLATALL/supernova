.onLoad <- function(...) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    vctrs::s3_register("ggplot2::autoplot", "pairwise")
    vctrs::s3_register("ggplot2::scale_type", "supernova_number")
  }
}
