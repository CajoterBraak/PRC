#' @title Extract a smooth PRC axis using LMMsolver (Boer, 2023)
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
#' @param axes matrix with previous axes. Default: NULL.
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
#' \code{x}, a column of \code{result$X} by the P-spline model
#' specified in \code{lmm_model}. This
#' is part of a power algorithm with block and Anderson acceleration to solve
#' the dominant eigen vector of the
#' underlying eigen problem while optimizing simultaneously the penalties
#' using \code{\link[LMMsolver]{LMMsolve}}.
#'
#' If \code{lmm_model$scaling =="ms"}, then in the result \code{B} in the
#' result is multiplied by \code{mult = sqrt(ncol(Y))} and all other items are
#' are divided by \code{mult = sqrt(ncol(Y))}, except \code{YB} to be inline
#' with \code{fitted(result$obj)}.
#' Note that \code{Yb}, a column in \code{result$YB} is the response in
#' \code{\link[LMMsolver]{LMMsolve}} model.
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
#' @noRd
#' @keywords internal
smoothPRC1 <- function(Y, lmm_model, options_iter = list(b_init =NULL,
                                                        k = 3, mA = 4,
                                                        tol = 1e-8, maxiter = 50),
                       axes=NULL) {
  normalizeb <- function(b){b/sqrt(sum(b^2))  }
  if (is.null(axes)) axes <- rep(1,nrow(Y))
  qr_prev_axes <- qr(axes)
  if (!spam::is.spam(Y)) Y <- as.matrix(Y)
#  TSS_Y<-sum(Y^2) - sum(nrow(Y)*colMeans(Y)^2)
#  Y<-Y/sqrt(TSS_Y)
  if (mean(Y>.Machine$double.eps) < 0.10 ) Y <- spam::as.spam(Y)
  n <- nrow(Y)
  m <- ncol(Y)

  # initial b
  if (is.null(options_iter$b_init)) b <- rnorm(m) else b <- options_iter$b_init
  objH0 <- NULL
  b <- normalizeb(b)

  # Anderson history
  b_hist <- list()
  r_hist <- list()

  # block-power buffer
  Xblock <- matrix(0, n, options_iter$k)
  Y <- as.matrix(Y)



  for (iter in 1:options_iter$maxiter) {

    b_old <- b

    ## ---- k block-power substeps ----
    for (j in 1:options_iter$k) {

      # latent response
      Yb <- as.numeric(Y %*% b)

      # spline fit
      obj <- LMMsolve(fixed = lmm_model$fixed,
                           spline = lmm_model$spline,
                           random  = lmm_model$random,
                           residual = lmm_model$residual,
                           data = cbind(lmm_model$data, Yb = Yb) )
      # extract spline latent axis xhat
        # spline fit under H0
        objH0 <-      LMMsolve(fixed = lmm_model$fixedH0,
                               spline = lmm_model$splineH0,
                               random  = lmm_model$random,
                               residual = lmm_model$residual,
                               data = cbind(lmm_model$data, Yb = Yb))
        xhat <- fitted(obj) - fitted(objH0)
       # xmean <-  - mean(xhat)
       # xhat <- xhat - xmean
        xhat <- qr.resid(qr_prev_axes, xhat)
      Xblock[, j] <- xhat
      # update b
      b_vec <- as.numeric(crossprod(Y, xhat))
      b <- normalizeb(b_vec)
    }

    ## ---- block SVD ----
    u1 <- svd(Xblock, nu=1, nv=0)$u[,1]
    b  <- as.numeric(crossprod(Y, u1))
    b  <- normalizeb(b)

    ## ---- Anderson acceleration ----
    r <- b - b_old
    b_hist <- c(b_hist, list(b_old))
    r_hist <- c(r_hist, list(r))

    if (length(r_hist) > options_iter$mA) {
      b_hist <- b_hist[(length(b_hist)-options_iter$mA+1):length(b_hist)]
      r_hist <- r_hist[(length(r_hist)-options_iter$mA+1):length(r_hist)]
    }

    ## ---- Anderson acceleration ----
    r <- b - b_old

    b_hist <- c(b_hist, list(b_old))
    r_hist <- c(r_hist, list(r))

    if (length(r_hist) > options_iter$mA) {
      b_hist <- b_hist[(length(b_hist)-options_iter$mA+1):length(b_hist)]
      r_hist <- r_hist[(length(r_hist)-options_iter$mA+1):length(r_hist)]
    }

    if (length(r_hist) == options_iter$mA) {

      Rmat <- do.call(cbind, r_hist)
      rhs  <- r_hist[[options_iter$mA]]

      alpha <- tryCatch(
        qr.solve(Rmat, rhs, tol = 1e-12),
        error = function(e) NULL
      )

      if (!is.null(alpha)) {

        b_new <- as.numeric(b - Rmat %*% alpha)

        if (all(is.finite(b_new)) &&
            sum(b_new^2) > .Machine$double.eps) {
          b <- normalizeb(b_new)
        }

      }
    }

    ## ---- convergence ----
    if (sqrt(sum((b - b_old)^2)) < options_iter$tol)
      break
  }
  PRC <-  get_PRC(obj, dat0 = lmm_model$dat0)
  obj$Yb <- Yb
  PRCstar <-  get_PRCstar(obj, dat0 = lmm_model$dat0)
  if (lmm_model$scaling =="ms") mult <- sqrt(length(b))else mult <- 1
  B <- as.matrix(b);colnames(B)<- "RDA1";rownames(B) <- colnames(Y)
  axes <- cbind(axes, xhat);
  colnames(axes)[ncol(axes)] <- paste0("RDA", ncol(axes)-1)
  out <-list(B = B*mult,
             X = xhat/mult,
             X_star = qr.resid(qr_prev_axes, (Yb - fitted(objH0)) )/mult,
             PRC = PRC/mult, PRCstar = PRCstar/mult, mult = mult,
             obj = obj, objH0=objH0, iter = iter, Y= Y, YB=Yb,
             lmm_model = lmm_model,options_iter = options_iter,
             axes = axes)
  for (nam in c("B", "X",   "X_star", "PRC", "PRCstar", "YB")) {
    out[[nam]] <- as.matrix(out[[nam]])
    colnames(out[[nam]]) <- paste0(nam,1:ncol(out[[nam]]))
  }
  out$obj <- list(out$obj)
  out$objH0 <- list(out$objH0)
  class(out) <- c("smoothPRC", "list")
  return(out)
}
#' @noRd
#' @keywords internal
get_PRC <-function(obj,dat0=NULL){
  # full fitted values
  xhat <- fitted(obj)
  pred0 <- predict(obj, newdata = dat0)
  PRC <- xhat - pred0$ypred # only the deviations from the control
  #xmean <-  - mean(xhat)
  return(PRC)
}
#' @noRd
#' @keywords internal
get_PRCstar <-function(obj,dat0=NULL){
  # full fitted values
  pred0 <- predict(obj, newdata = dat0)
  PRCstar <- obj$Yb - pred0$ypred # only the deviations from the control
  #xmean <-  - mean(xhat)
  return(PRCstar)
}
