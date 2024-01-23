#' @title Biomonitoring example from van den Brink et al. (2009)
#'

#' @description
#' The data frame \code{biomonitoring} contains the design and species data from van den Brink et al.(2009). The data is
#' on biomonitoring of macroinvertebrate species in the Rhine and Meuse rivers, near the
#' towns of Kampen and Grave, respectively, from 1992 - 2000, sampled six times per year,
#' except in 1998 (4x), 1999 and 2000 (7x). The first example uses reference coding of an reduncancy analysis without covariates
#' so as to emphasize the compositional difference from a reference. The second example is a more regular PRC, contrasting
#' the compositional changes at one site without those in another.
#' \itemize{
#' \item \code{sample} character identifier for the sample.
#' \item \code{year} numeric year of sampling
#' \item \code{location} location of sampling: factor with levels c("Grave","Kampen")
#' \item counts of 139 macroinvertebrate species
#' }
#' @name biomonitoring
#' @docType data

#' @references
#'
#' van den Brink, PJ, den Besten, PJ, bij de Vaate, A and ter Braak, CJF, 2009.
#' Principal response curves technique for the analysis of multivariate biomonitoring time series.
#' Environmental Monitoring and Assessment. 152, 271-281. http://dx.doi.org/10.1007/s10661-008-0314-6
NULL
