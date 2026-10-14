#' @title Permutation Test for Smooth PRC

#' @param object an object from \code{\link{smoothPRC}}.
#' @param permutations the number of
#' permutations required (default 19), a list of control values for the permutations as
#' returned by the function \code{\link[permute]{how}} or a permutation matrix where each row
#' gives the permuted indices.
#' @param verbose logical for printing progress of the permutations
#' @param ... unused.
#' @details
#' The test takes account of additional covariates in \code{fixed} of
#' the LLMsolver model. The test is approximate without permutation of whole
#' time series.
#' The test is based on the first axis.
#' @export
anova.smoothPRC <- function(object,
                            ...,
                            permutations = 19, verbose = TRUE) {
  an <- if (inherits(object,"powerSVD")){
    anova_powerSVD(object, permutations = permutations, verbose = verbose)
 } else {
   anova_powerEig(object, permutations = permutations, verbose = verbose)
 }
  invisible(an)
}
