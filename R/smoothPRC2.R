#' @title Add a smooth PRC axis to a smoothPRC object
#' @inheritParams smoothPRC
#' @param smooth_PRC_axis1 result of \code{\link{smoothPRC}}.
#' @details
#' The first axis of a smooth PRC model is add to the fixed formulas, so
#' as to obtain the second axis.
#' By default, \code{k} and \code{mA} in \code{option_iter} are doubled compared
#' to the values in \code{smooth_PRC_axis1}, as the separation with the third
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
smoothPRC2 <- function(smooth_PRC_axis1,
                       options_iter =  NULL
                       ) {
  if (!inherits(smooth_PRC_axis1,"smoothPRC")) errorCondition(
      "smooth_PRC_axis1 must be the result of smoothPRC")
  if (is.null(options_iter)) {
    options_iter <- smooth_PRC_axis1$options_iter
  }
  smooth_PRC2 <- smoothPRC1(Y            = smooth_PRC_axis1$Y,
                            lmm_model    = smooth_PRC_axis1$lmm_model,
                            options_iter = options_iter,
                            axes         = smooth_PRC_axis1$axes)
  # help function
  addaxis <- function(nam){
    axes <- cbind(smooth_PRC_axis1[[nam]], smooth_PRC2[[nam]]);
    colnames(axes) <- paste0(nam, 1:ncol(axes))
    return(axes)
  }
  for (nam in c("B", "X",   "X_star", "PRC", "PRC_star", "YB")) {
    smooth_PRC2[[nam]] <- addaxis(nam)
  }
  smooth_PRC2$obj <- c(smooth_PRC_axis1$obj,  smooth_PRC2$obj)
  smooth_PRC2$objH0 <- c(smooth_PRC_axis1$objH0,  smooth_PRC2$objH0)
  return(smooth_PRC2)
}
