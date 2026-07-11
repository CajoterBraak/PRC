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
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
#' @export
smoothPRC <- function(Y, lmm_model, options_iter = list(b_init =NULL,
                                                        k = 3, mA = 4,
                                                        tol = 1e-8, maxiter = 50)) {

  normalizeb <- function(b){b/sqrt(sum(b^2))  }
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
        xmean <-  - mean(xhat)
        xhat <- xhat - xmean
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

    if (length(r_hist) == options_iter$mA) {
      Rmat <- do.call(cbind, r_hist)
      rhs  <- r_hist[[options_iter$mA]]
      # lambda <- 1e-10
      # alpha <- solve(crossprod(Rmat) + lambda * diag(ncol(Rmat)),
      #                crossprod(Rmat, rhs))
      alpha <- qr.solve(Rmat, rhs, tol = 1e-12)
      b_new <- b - Rmat %*% alpha
      b <-  normalizeb(b_new)
    }

    ## ---- convergence ----
    if (sqrt(sum((b - b_old)^2)) < options_iter$tol)
      break
  }
  PRC <-  get_PRC(obj, dat0 = lmm_model$dat0)
  obj$Yb <- Yb
  PRCstar <-  get_PRCstar(obj, dat0 = lmm_model$dat0)
  if (lmm_model$scaling =="ms") mult <- sqrt(length(b))else mult <- 1
  B <- as.matrix(b*mult);colnames(B)<- "RDA1";rownames(B) <- colnames(Y)

  out <-list(b = B,
             x = xhat/mult,
             x_star = (Yb - fitted(objH0))/mult,
             PRC = PRC/mult, PRCstar = PRCstar/mult, mult = mult,
             obj = obj, objH0=objH0, iter = iter,Y= Y, Yb=Yb,
             lmm_model = lmm_model,options_iter = options_iter)
  class(out) <- c("smoothPRC", "list")
  return(out)
}
get_PRC <-function(obj,dat0=NULL){
  # full fitted values
  xhat <- fitted(obj)
  pred0 <- predict(obj, newdata = dat0)
  xhat <- xhat - pred0$ypred # only the deviations from the control
  #xmean <-  - mean(xhat)
  return(xhat)
}
get_PRCstar <-function(obj,dat0=NULL){
  # full fitted values
  pred0 <- predict(obj, newdata = dat0)
  xstar <- obj$Yb - pred0$ypred # only the deviations from the control
  #xmean <-  - mean(xhat)
  return(xstar)
}
