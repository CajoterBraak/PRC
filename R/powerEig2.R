#' @title Add an eigen vector axis to a powerEig object
#' @inheritParams smoothPRC
#' @param pwrEig_axis result of \code{\link{smoothPRC}}.
#' @details
#' The first axis of a smooth PRC model is add to the fixed formulas, so
#' as to obtain the second axis.
#' By default, \code{k} and \code{mA} in \code{option_iter} are doubled compared
#' to the values in \code{pwrEig_axis}, as the separation with the third
#' axis is likely small, which makes convergence slower.
#' @importFrom LMMsolver LMMsolve spl1D spl2D spl3D
#' @importFrom stats update
#' @noRd
#' @keywords internal
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
powerEig2 <- function(pwrEig_axis,
                       options_iter =  NULL,
                       fit_state = NULL
                       ) {
  if (!inherits(pwrEig_axis,"powerEig")) errorCondition(
      "pwrEig_axis must be the result of powerEig")
  if (is.null(options_iter)) {
    options_iter <- pwrEig_axis$options_iter
  }
  pwrEig2 <- powerEig_axis(A             = pwrEig_axis$A,
                           model        = pwrEig_axis$model,
                           options_iter = options_iter,
                           fit_state    = pwrEig_axis$fit_state,
                           weights      = pwrEig_axis$weights,
                           axes         = pwrEig_axis$axes,
                           n_covariates = pwrEig_axis$n_covariates)
  # help function
  addaxis <- function(nam){

    axs <- cbind(pwrEig_axis[[nam]], pwrEig2[[nam]]);
    colnames(axs) <- paste0(nam, 1:ncol(axs))
    return(axs)
  }
  for (nam in c("eig", "X",   "X_star","iter", "iA", "convergence")) {
    pwrEig2[[nam]] <- addaxis(nam)
  }
  pwrEig2$obj <- c(pwrEig_axis$obj,  pwrEig2$obj)
  pwrEig2$objH0 <- c(pwrEig_axis$objH0,  pwrEig2$objH0)
  return(pwrEig2)
}
