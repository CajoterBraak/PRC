#' @title Smooth PRC using LMMsolver (Boer, 2023)
#' @param Y matrix, data frame of the abundance data
#' (dimension \emph{sites} x \emph{species})
#' @param lmm_model The result of \code{\link{set_smoothPRC_model}}.
#' @param options_iter list of five elements
#' \itemize{
#' \item \code{b_init}: initial values of the species loadings.
#' Default \code{NULL}.
#' \item \code{k}: number of cycles in block-power updating to  better separate
#' the first eigen vector from the second. Default \code{NULL}.
#' \item \code{mA}: number of vectors in Anderson acceleration. Default \code{3}.
#' \item \code{tol}: stopping criterion, tolarance. Default \code{1e-8}.
#' \item \code{maxiter}:maximum number of iterations. Default \code{50}.
#' }
#' @param n_axes the number of axes to extract. Default \code{2}.
#' @details
#' An atypical aspect of the function is that the time and treatment factors
#' need to have the names Time and Treatment in \code{data}.
#'
#' Classical PRC is a reduced rank model of rank 1 (or 2) with model
#' Treatment*Time with the Time factor is a concomitant variable.
#' In smoothPRC the factor Time is taken quantitatively and the main effect
#' of time is not an un-ordered set of values but a smooth P-spline model.
#' Also, the time-dependent effects of the treatments are smooth P-splines.
#' As the main and interaction effects are splines, the main effect of time
#' cannot be adjusted for in the usual way. It is taken out via a separate
#' spline fit using the formulas in \code{splineH0} and \code{fixedH0} of
#' \code{\link{set_smoothPRC_model}}.
#'
#' The algorithm solves a penalized reduced rank model for the
#' dominant eigen vector (\emph{i.e.} rank 1).
#' The model is a P-spline model and
#' the penalty parameters are obtained by REML using
#' \code{\link[LMMsolver]{LMMsolve}} (Boer 2023).
#' The model is linear in, possibly tensor, B-splines,
#' but through the B-splines non-linear from the perspective of the user. With
#' quadratic ridge-type difference penalties on the coefficients the B-splines
#' becomeP-splines (penalized B-splines).
#' For an initial \code{b} of species scores, the unconstrainted sites \code{Yb}
#' are calculated, which are then used as response vector in
#' \code{\link[LMMsolver]{LMMsolve}} to give a constrained site scores
#' \code{x} by the P-spline model specified in \code{lmm_model}. This
#' is part of a power algorithm with block and Anderson acceleration to solve
#' the dominant eigen vector of the
#' underlying eigen problem while optimizing simultaneously the penalties
#' using \code{\link[LMMsolver]{LMMsolve}}.
#'
#' If \code{lmm_model$scaling =="ms"}, then in the result \code{B} in the
#' result is multiplied by \code{mult = sqrt(ncol(Y))} and all other items are
#' are divided by \code{mult = sqrt(ncol(Y))}, except \code{Yb} to be inline
#' with \code{fitted(result$obj)}.
#' Note that \code{Yb} is the response in
#' \code{\link[LMMsolver]{LMMsolve}} model.
#' @return A list.
#' The eigenvalues are in \code{result$eig}.
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
#' @importFrom stats var
#' @export
smoothPRC <- function(Y, lmm_model, options_iter = list(b_init =NULL,
                                                        k = 3, mA = 4,
                                                        tol = 1e-8, maxiter = 50), n_axes = 2) {


  smooth_PRC1 <- smoothPRC1(Y            = Y,
                            lmm_model    = lmm_model,
                            options_iter = options_iter,
                            axes         = NULL)
    iaxis = 1
    val1  <-var(smooth_PRC1$X[,iaxis])
    #options_iter$k <- 1.5 * options_iter$k
    #options_iter$mA <- 0

    while(iaxis < n_axes && var(smooth_PRC1$X[,iaxis]) > 1.0e-2*val1){
      smooth_PRC2 <- smoothPRC2(smooth_PRC_axis1 = smooth_PRC1,
                             options_iter =  options_iter)
      smooth_PRC1 <- smooth_PRC2
      iaxis <- iaxis+1
    }
    smooth_PRC1$eig <- ncol(Y)* apply(smooth_PRC1$X,2, var)
  return(smooth_PRC1)
}
