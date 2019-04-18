# Re-export useful functions from packages
#
# The headings in this file match the headings on the R cheatsheet
# see https://github.com/UCLATALL/czi-stats-course-files

# Tables ------------------------------------------------------------------

#' @importFrom mosaicCore count
#' @export
mosaicCore::count

#' @importFrom mosaicCore tally
#' @export
mosaicCore::tally


# Simple Statistics -------------------------------------------------------

#' @importFrom mosaic cor
#' @export
mosaic::cor

#' @importFrom mosaic cov
#' @export
mosaic::cov

#' @importFrom mosaic favstats
#' @export
mosaic::favstats

#' @importFrom mosaic IQR
#' @export
mosaic::IQR

#' @importFrom mosaic mean
#' @export
mosaic::mean

#' @importFrom mosaic mean_
mosaic::mean_

#' @importFrom mosaic median
#' @export
mosaic::median

#' @importFrom mosaic max
#' @export
mosaic::max

#' @importFrom mosaic min
#' @export
mosaic::min

#' @importFrom mosaic prod
#' @export
mosaic::prod

#' @importFrom mosaic range
#' @export
mosaic::range

#' @importFrom mosaic sd
#' @export
mosaic::sd

#' @importFrom mosaic sum
#' @export
mosaic::sum

#' @importFrom mosaic var
#' @export
mosaic::var

#' @importFrom lsr cohensD
#' @export
lsr::cohensD


# Fitting & Evaluating Models ---------------------------------------------

#' @importFrom mosaicCore makeFun
#' @export
mosaicCore::makeFun


# Probability Distributions -----------------------------------------------

#' @importFrom MASS fitdistr
#' @export
MASS::fitdistr

#' @importFrom mosaicCore fit_distr_fun
#' @export
mosaicCore::fit_distr_fun

#' @importFrom mosaic xcnorm
#' @export
mosaic::xcnorm

#' @importFrom mosaic xpnorm
#' @export
mosaic::xpnorm

#' @importFrom mosaic xqnorm
#' @export
mosaic::xqnorm

#' @importFrom mosaic xcf
#' @export
mosaic::xcf

#' @importFrom mosaic xpf
#' @export
mosaic::xpf

#' @importFrom mosaic xqf
#' @export
mosaic::xqf

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

#' @importFrom ggstance GeomBoxploth
ggstance::GeomBoxploth

#' @importFrom ggstance StatBoxploth
ggstance::StatBoxploth

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
ggformula::gf_dhistogram

#' @importFrom ggstance StatBinh
ggstance::StatBinh

#' @importFrom ggstance GeomBarh
ggstance::GeomBarh

#' @importFrom ggstance stat_binh
#' @export
ggstance::stat_binh

#' @importFrom ggstance geom_barh
#' @export
ggstance::geom_barh

#' @importFrom ggstance position_stackv
#' @export
ggstance::position_stackv

#' @importFrom ggformula gf_dhistogram
#' @export
ggformula::gf_histogramh

#' @importFrom ggformula gf_dhistogramh
#' @export
ggformula::gf_dhistogramh

#' @importFrom ggformula StatFitdistr
ggformula::StatFitdistr

#' @importFrom ggformula gf_fitdistr
#' @export
ggformula::gf_fitdistr

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

#' @importFrom ggformula StatLm
ggformula::StatLm

#' @importFrom ggformula GeomLm
ggformula::GeomLm

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
