#' Principal Response Curves (PRC) for displaying multivariate main effects and
#' interactions
#' @aliases PRC-package
#' @name PRC-package
#' @description
#' PRC creates 1d and 2d PRC-diagrams using Principal Response Curves analysis
#' (van den Brink & ter Braak 1999).
#' The main user functions are \code{\link{doPRC}}, \code{\link{plotPRC}} and
#' \code{\link[PRC]{plotPRC2d}}.
#' The easiest example is the code demo \code{PRC_pyrifos}.
#' The function \code{\link{doPRC}} is more general
#' than and replaces \code{vegan::}\code{\link[vegan]{prc}}.
#'
#' @references
#' ter Braak, C.J.F. (2023) Redundancy analysis includes analysis of
#' variance-simultaneous component analysis (ASCA)
#' and outperforms its extensions
#' Chemometrics and Intelligent Laboratory Systems, 240
#' \doi{10.1016/j.chemolab.2023.104898}
#'
#' van den Brink, P.J. & ter Braak, C.J.F. (1999) Principal Response Curves:
#' Analysis of
#' time-dependent multivariate responses of a biological community to stress.
#' Environmental Toxicology and Chemistry, 18, 138-148.
#' \doi{10.1002/etc.5620180207}
#'
#' Oksanen, J., et al. (2022)
#' vegan: Community Ecology Package. R package version 2.6-4.
#' \url{https://CRAN.R-project.org/package=vegan}.
#'
#' @seealso \code{\link{doPRC}}, \code{\link{smoothPRC}}, \code{\link[vegan]{prc}}
#' @keywords internal
#' @importFrom LMMsolver LMMsolve
#' @importFrom stats as.formula delete.response fitted formula model.matrix nobs predict rnorm terms
#' @importFrom rlang .data
NULL
