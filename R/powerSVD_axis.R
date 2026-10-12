#' @title Extract an axis by the two-sided power algorithm
#' @inheritParams powerSVD
#' @importFrom stats fitted
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
#' @noRd
#' @keywords internal
powerSVD_axis <- function(Y, model,
                       weights= NULL,
                       qr_prev_axes = NULL,
                       axes= NULL,
                       qr_covariates_columns = NULL,
                       n_covariates = 1,
                       options_iter = list(init =NULL,
                                           k = 3, mA = 4,
                                           tol = 1e-8, maxiter = 50),
                       fit_state = NULL
                       ) {
  if (is.null(fit_state)) fit_state <-
      list(rows = NULL,rows0 = NULL, columns = NULL, columns0 = NULL)



  # print("axes")
  # print(axes)
  # print("end axes")
  n <- nrow(Y)
  m <- ncol(Y)
  if (is.null(weights)) weights <- set_weights(Y, model$method)
  if (is.null(qr_prev_axes)) qr_prev_axes <- qr(axes*weights$sqrtR)

  # internal functions
  normalizeb <- function(b, sqrtw=1){b/sqrt(sum((sqrtw*b)^2))  }
  get_fitted_H0 <- function(objH0,n) {
    if (is.null(objH0)) numeric(n)
    else fitted(objH0)
  }
  iaxis <- if (is.null(axes)) 1 else ncol(axes)-n_covariates + 1
  # initial b
  if (is.null(options_iter$init)) b <- drop(initialize_pwr_alg(model, Y = Y,
                                      weights =weights, axes = axes)) else {
    b <- as.matrix(options_iter$init)[,iaxis]
    if (!length(b)== ncol(Y) )
      stop("wrong length of initial values in options_iter$init")
    }

  b <- normalizeb(b, sqrtw = weights$sqrtK)

  # Anderson history
  b_hist <- list()
  r_hist <- list()
  # block-power buffer
  Xblock <- matrix(0, n, options_iter$k)
  xhat <- rep(0, nrow(Y))

  for (iter in 1:options_iter$maxiter) {

    b_old <- b
    x_old <- xhat
    ## ---- k block-power substeps ----
    for (j in 1:options_iter$k) {

      # latent response
      xstar <- (Y %*% b) / weights$R
      xstar <- qr.resid(qr_prev_axes, weights$sqrtR * xstar) / weights$sqrtR
      # row model spline fit
      #print(range(xstar))
      attr(xstar, "iaxis") <- iaxis
      obj   <- fit_full_model_rows(model, y = xstar, weights = weights,
                        axes = axes, options_fit = list(state = fit_state$rows))
      fit_state$rows <- update_fit_state(obj, state = fit_state$rows)
      #print("stored state")
      #str(fit_state$rows)
      objH0 <- fit_null_model_rows(model, y = xstar, weights = weights,
                        axes = axes, options_fit = list(state = fit_state$rows0))
      fit_state$rows0 <- update_fit_state(objH0, state = fit_state$rows0)
      xhat <- fitted(obj) - get_fitted_H0(objH0, length(xstar))
      # This centers xhat, corresponding to (C)CA and centered PCA/RDACCA!
      xhat <- qr.resid(qr_prev_axes, weights$sqrtR*xhat)/weights$sqrtR

      Xblock[, j] <- xhat
      # update b
      b_vec <- crossprod(Y, xhat)/ weights$K
      ## column model, if any, or identity_fit
      obj_col   <- fit_full_model_columns(model, y = b_vec, weights = weights,
                                options_fit = list(state = fit_state$columns))
      fit_state$columns <- update_fit_state(obj_col, state = fit_state$columns)
      if (!inherits(obj_col, "identity_fit") ){
        objH0_col <- fit_null_model_columns(model, y = b_vec, weights = weights,
                                options_fit = list(state = fit_state$columns0))
        fit_state$columns0 <- update_fit_state(objH0_col, state = fit_state$columns0)
        b_vec <- fitted(obj_col) - get_fitted_H0(objH0_col, length(b_vec))


      }
      #  no centering or othogonalizing if there are no column covariates
      if (!is.null(qr_covariates_columns)){
        b_vec <- qr.resid(qr_covariates_columns, weights$sqrtK*b_vec)/
                                                              weights$sqrtK
      }


      b <- normalizeb(b_vec, sqrtw = weights$sqrtK)
    }

   # ---- block weighted SVD ----
   u1 <- svd(Xblock*weights$sqrtR, nu=1, nv=0)$u[,1]
   b  <- crossprod(Y, u1/weights$sqrtR) / weights$K
   b  <- normalizeb(b, sqrtw = weights$sqrtK)

   ## ---- convergence ----
   db <- sum( (weights$sqrtK*(b - b_old))^2)
   dx <-  sum((weights$sqrtR*(xhat - x_old))^2)#/
   #max(sum((weights$sqrtR * xhat)^2), .Machine$double.eps)
   if (max(db, dx) <  options_iter$tol) break
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
          b <- normalizeb(b_new, sqrtw = weights$sqrtK)
        }

      }
    }
  }
  eig <- sum( (weights$sqrtR*xhat)^2)
  eig <- if(model$method == "rda") eig/(n-1) else eig
  mult <-if ( model$method == "rda") sqrt(length(b)) else 1
  B <- as.matrix(b);colnames(B)<- "B1";rownames(B) <- colnames(Y)
  axes <- cbind(axes, xhat);
  colnames(axes)[ncol(axes)] <- paste0(model$method, ncol(axes)-n_covariates)
  out <-list(eig=eig, B = B*mult,
             X = xhat/mult,
             X_star = qr.resid(qr_prev_axes,
                weights$sqrtR*(xstar - get_fitted_H0(objH0, length(xstar)) ))/
               (weights$sqrtR*mult),
             #YB=xstar,
             mult = mult,
             weights = weights,
             obj = list(obj), objH0= list(objH0), iter = iter, Y= Y,
             model = model,options_iter = options_iter,
             axes = axes,  n_covariates =  n_covariates,
             qr_covariates_columns = qr_covariates_columns,
             fit_state = fit_state, convergence = c(delta_b = db, delta_x= dx))
  for (nam in c("B", "X", "X_star")) {
    out[[nam]] <- as.matrix(out[[nam]])
    colnames(out[[nam]]) <- paste0(nam,1:ncol(out[[nam]]))
  }
  class(out) <- c("powerSVD", "list")
  return(out)
}
# define generics

#' Fit the full row model (internal)
#'
#' Returns an object with a fitted() method.
#'
#' @param model Model specification.
#' @param ... Additional arguments.
#' @export
fit_full_model_rows <- function(model,  ...)
  UseMethod("fit_full_model_rows")
#'
#' Fit the full column model (internal)
#'
#' Returns an object with a fitted() method.
#
#' @param model Model specification.
#' @param ... Additional arguments.
#' @export
fit_full_model_columns <- function(model,  ...)
  UseMethod("fit_full_model_columns")
#'
#' Fit the null row model (internal)
#'
#' Returns an object with a fitted() method.
#'
#' @param model Model specification.
#' @param ... Additional arguments.
#' @export
fit_null_model_rows <- function(model, ...)
  UseMethod("fit_null_model_rows")
#' Fit the null column model (internal)
#'
#' Returns an object with a fitted() method.
#'
#' @param model Model specification.
#' @param ... Additional arguments.

#' @export
fit_null_model_columns <- function(model, ...)
  UseMethod("fit_null_model_columns")
#'
#' @noRd
#' @keywords internal
#' @export
fit_full_model_columns.default <- function(model, ..., y) identity_fit(y)
#'
#' @noRd
#' @keywords internal
#' @export
fit_full_model_rows.default <- function(model,  ..., y) identity_fit(y)
#'
#' @noRd
#' @export
fit_null_model_rows.default <- function(model,  ..., y) NULL
#'
#' @noRd
#' @keywords internal
#' @export
fit_null_model_columns.default <- function(model, ..., y) NULL

#' @noRd
#' @keywords internal
identity_fit <- function(y)
{
  structure(
    list(
      fitted = y,
      df = length(y) - 1
    ),
    class = c("identity_fit")
  )
}
#' Updates the parameters of solver (internal)
#' @param object Object resulting for the fit.
#' @param state Optional list of arguments for warm starts of the solver
#' @export
update_fit_state <- function(object, state)
  UseMethod("update_fit_state")
#' @keywords internal
#' @export
update_fit_state.default <- function(object, state = NULL)
{
  state
}
#' initializes the power algorthm (internal)
#' @param model Model specification.
#' @param Y response
#' @param weights optional weights
#' @param axes prior axes and covariable data
#' @export
initialize_pwr_alg <- function(model, Y, weights, axes)
UseMethod("initialize_pwr_alg", model)
#' @noRd
#' @keywords internal
#' @export
initialize_pwr_alg.default <- function(model, Y, weights, axes)
{
  print("default random initialization ")
  n <- if ( inherits(Y, "dist") ) attr(Y,"Size") else ncol(Y)
  rnorm(n)
}
