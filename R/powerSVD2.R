#' @title Add an eigen vector axis to a powerSVD object
#' @inheritParams smoothPRC
#' @param pwrSVD_axis result of \code{\link{smoothPRC}}.
#' @details
#' The first axis of a smooth PRC model is add to the fixed formulas, so
#' as to obtain the second axis.
#' By default, \code{k} and \code{mA} in \code{option_iter} are doubled compared
#' to the values in \code{pwrSVD_axis}, as the separation with the third
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
powerSVD2 <- function(pwrSVD_axis,
                       options_iter =  NULL,
                       fit_state = NULL
                       ) {
  if (!inherits(pwrSVD_axis,"powerSVD")) errorCondition(
      "pwrSVD_axis must be the result of powerSVD")
  if (is.null(options_iter)) {
    options_iter <- pwrSVD_axis$options_iter
  }
  pwrSVD2 <- powerSVD_axis(Y            = pwrSVD_axis$Y,
                            model    = pwrSVD_axis$model,
                            options_iter = options_iter,
                            fit_state =   pwrSVD_axis$fit_state,
                            weights      = pwrSVD_axis$weights,
                            axes         = pwrSVD_axis$axes,
                   qr_covariates_columns = pwrSVD_axis$qr_covariates_columns,
                            n_covariates = pwrSVD_axis$n_covariates)
  # help function
  addaxis <- function(nam){
    axs <- cbind(pwrSVD_axis[[nam]], pwrSVD2[[nam]]);
    colnames(axs) <- paste0(nam, 1:ncol(axs))
    return(axs)
  }
  for (nam in c("eig","B", "X",   "X_star","iter")) {
    pwrSVD2[[nam]] <- addaxis(nam)
  }
  pwrSVD2$obj <- c(pwrSVD_axis$obj,  pwrSVD2$obj)
  pwrSVD2$objH0 <- c(pwrSVD_axis$objH0,  pwrSVD2$objH0)
  return(pwrSVD2)
}
