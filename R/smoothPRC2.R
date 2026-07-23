#' @title Second axis of smooth PRC using LMMsolver
#' @inheritParams smoothPRC
#' @param smooth_PRC_axis1 result of \code{\link{smoothPRC}}.
#' @details
#' The first axis of a smooth PRC model is add to the fixed formulas, so
#' as to obtain the second axis.
#' @importFrom LMMsolver LMMsolve spl1D spl2D spl3D
#' @importFrom stats update
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
#' @export
smoothPRC2 <- function(smooth_PRC_axis1,
                       options_iter =  NULL
                       ) {
  if (!inherits(smooth_PRC_axis1,"smoothPRC")) errorCondition(
      "smooth_PRC_axis1 must be the result of smoothPRC")
  if (is.null(options_iter)) options_iter <- smooth_PRC_axis1$options_iter
  Y <- smooth_PRC_axis1$Y
  lmm_model <- smooth_PRC_axis1$lmm_model
  options_iter  <- smooth_PRC_axis1$options_iter
  axis1 <- smooth_PRC_axis1$x
  lmm_model$data$axis1 <- axis1
  lmm_model$dat0$axis1 <- axis1
  lmm_model$fixed <- update(lmm_model$fixed, . ~ . + axis1)
  lmm_model$fixedH0 <- update(lmm_model$fixedH0, . ~ . + axis1)
  smooth_PRC2 <- smoothPRC(Y=Y, lmm_model= lmm_model,
                                options_iter= options_iter)
  return(smooth_PRC2)
}
