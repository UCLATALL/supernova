# Re-export useful functions from packages
#
# The headings in this file match the headings on the R cheatsheet
# see https://github.com/UCLATALL/czi-stats-course-files

# Tables ------------------------------------------------------------------

#' @importFrom mosaicCore tally
#' @export
mosaicCore::tally


# Simple Statistics -------------------------------------------------------

#' @importFrom mosaic mean
#' @export
mosaic::mean

#' @importFrom mosaic median
#' @export
mosaic::median

#' @importFrom mosaic var
#' @export
mosaic::var

#' @importFrom mosaic sd
#' @export
mosaic::sd

#' @importFrom mosaic favstats
#' @export
mosaic::favstats

#' @importFrom lsr cohensD
#' @export
lsr::cohensD

#' @importFrom mosaic cor
#' @export
mosaic::cor


# Fitting & Evaluating Models ---------------------------------------------

#' @importFrom mosaicCore makeFun
#' @export
mosaicCore::makeFun


# Probability Distributions -----------------------------------------------

#' @importFrom mosaic xpnorm
#' @export
mosaic::xpnorm

#' @importFrom mosaic zscore
#' @export
mosaic::zscore


# Data --------------------------------------------------------------------

#' @importFrom dplyr select
#' @export
dplyr::select

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @importFrom dplyr arrange
#' @export
dplyr::arrange

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom dplyr recode
#' @export
dplyr::recode


# Simulation & Resampling -------------------------------------------------

#' @importFrom mosaic sample
#' @export
mosaic::sample

#' @importFrom mosaic resample
#' @export
mosaic::resample

#' @importFrom mosaic do
#' @export
mosaic::do

#' @importFrom mosaic shuffle
#' @export
mosaic::shuffle


# Plots -------------------------------------------------------------------

#' @importFrom ggformula gf_bar
#' @export
ggformula::gf_bar

#' @importFrom ggformula gf_boxplot
#' @export
ggformula::gf_boxplot

#' @importFrom ggformula gf_boxploth
#' @export
ggformula::gf_boxploth

#' @importFrom ggformula gf_density
#' @export
ggformula::gf_density

#' @importFrom ggformula gf_histogram
#' @export
ggformula::gf_histogram

#' @importFrom ggformula gf_histogramh
#' @export
ggformula::gf_histogramh

#' @importFrom ggformula gf_dhistogram
#' @export
ggformula::gf_dhistogram

#' @importFrom ggformula gf_dhistogramh
#' @export
ggformula::gf_dhistogramh

#' @importFrom ggformula gf_point
#' @export
ggformula::gf_point

#' @importFrom ggformula gf_jitter
#' @export
ggformula::gf_jitter

#' @importFrom ggformula gf_dist
#' @export
ggformula::gf_dist

#' @importFrom ggformula gf_lm
#' @export
ggformula::gf_lm

#' @importFrom ggformula gf_abline
#' @export
ggformula::gf_abline

#' @importFrom ggformula gf_hline
#' @export
ggformula::gf_hline

#' @importFrom ggformula gf_vline
#' @export
ggformula::gf_vline

#' @importFrom ggformula gf_facet_grid
#' @export
ggformula::gf_facet_grid

#' @importFrom ggformula gf_facet_wrap
#' @export
ggformula::gf_facet_wrap

#' @importFrom ggformula gf_labs
#' @export
ggformula::gf_labs
