#' @title Extract an axis by the two-sided power algorithm
#' @inheritParams powerSVD
#' @param A a symmetric semi-positive definite matrix.  Starting from
#' a matrix of squared distinces D, D is double centred by f, then
#' A = -f(D)/2. In case of Euclidean squared distance, calculated from X
#' then A = XX'
#' double centred distance matrix
#' @importFrom stats fitted
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
#' @noRd
#' @keywords internal
powerEig_axis <- function(A, model,
                          weights = NULL,
                          qr_prev_axes = NULL,
                          axes = NULL,
                          n_covariates = 1,
                          options_iter = list(
                            init = NULL,
                            k = 3,
                            mA = 4,
                            tol = 1e-8,
                            maxiter = 50
                          ),
                          fit_state = NULL)
{
  if (is.null(fit_state)) fit_state <-
      list(rows = NULL,rows0 = NULL)



  #A <- as.matrix(A)
  n <- nrow(model$data)
  iA <- 0
  if (is.null(weights)) weights <- list(R =rep(1, n))


  if (is.null(qr_prev_axes))
    qr_prev_axes <- qr(axes )

  normalizex <- function(x) x / sqrt(sum(x^2))

  get_fitted_H0 <- function(objH0, n) {
    if (is.null(objH0)) numeric(n)
    else fitted(objH0)
  }
  iaxis <- if (is.null(axes)) 1 else ncol(axes) - n_covariates + 1
  ## initial x
  if (is.null(options_iter$init)) x <- drop(initialize_pwr_alg(model, Y = A,
                                                weights = weights, axes = axes))
  else {
    x <- as.matrix(options_iter$init)[,iaxis]
    if (!length(x)== n )
      stop("wrong length of initial values in options_iter$init")
  }
  x <- normalizex(x)

  x_hist <- list()
  r_hist <- list()

  Xblock <- matrix(0, n, options_iter$k)

  for (iter in seq_len(options_iter$maxiter)) {

    x_old <- x

    ## block-power substeps
    for (j in seq_len(options_iter$k)) {

      #z <- A %*% x
      z <- A_mult(A, x)
      z <- qr.resid(qr_prev_axes,z)


      attr(z, "iaxis") <- iaxis
      obj   <- fit_full_model_rows(model, y = z, weights = weights,
                       axes = axes, options_fit = list(state = fit_state$rows))

      fit_state$full <- update_fit_state(
        obj, state = fit_state$full
      )
      objH0   <- fit_null_model_rows(model, y = z, weights = weights,
                        axes = axes, options_fit = list(state = fit_state$rows0))

      fit_state$null <- update_fit_state(
        objH0, state = fit_state$null
      )

      x <- fitted(obj) -
        get_fitted_H0(objH0, length(z))

      if (!is.null(qr_prev_axes)){
        x <- xhat <- qr.resid(
          qr_prev_axes,
           x
        )
      } else xhat <- x


      x <- normalizex(x)

      Xblock[, j] <- x
    }

    ## block-power acceleration
    u1 <- svd(Xblock, nu = 1,nv = 0)$u[, 1]

    x <- normalizex(u1)


    ## convergence
    dx <- sum(
      ((x - x_old))^2
    )

    if (dx < options_iter$tol)
      break

    ## Anderson acceleration
    r <- x - x_old

    x_hist <- c(x_hist, list(x_old))
    r_hist <- c(r_hist, list(r))

    if (length(r_hist) > options_iter$mA) {
      keep <- (length(r_hist) -
                 options_iter$mA + 1):length(r_hist)
      x_hist <- x_hist[keep]
      r_hist <- r_hist[keep]
    }

    if (length(r_hist) == options_iter$mA) {

      Rmat <- do.call(cbind, r_hist)
      rhs <- r_hist[[options_iter$mA]]

      alpha <- tryCatch(
        qr.solve(Rmat, rhs, tol = 1e-12),
        error = function(e) NULL
      )

      if (!is.null(alpha)) {

        x_new <- as.numeric(
          x - Rmat %*% alpha
        )

        if (all(is.finite(x_new)) && sum(x_new^2) > .Machine$double.eps){
          iA <- iA+1
          x <- normalizex(x_new)}
      }
    }
  }
  eig <- sum(xhat * x)
  #eig <- sum(xhat * x)*(n/(n-1))# for Euclidean
  #       to get sample variance (as in princomp and vegen::rda)
  axes <- cbind(axes, x)
  colnames(axes)[ncol(axes)] <-
    paste0("eig", ncol(axes) - n_covariates)
  out <- list(
    eig = eig,
    X   = as.matrix(x)*sqrt(eig),
    X_star =as.matrix(qr.resid(qr_prev_axes,
                     (z - get_fitted_H0(objH0, length(z)) ))/sqrt(eig)),
    mult = 1,
    obj = list(obj),
    objH0 = list(objH0),
    iter = iter,
    A = A,
    model = model,options_iter = options_iter,
    axes = axes,  n_covariates =  n_covariates,
    fit_state = fit_state,
    iA = iA,
    convergence = c(delta_x = dx)

  )
  class(out) <- c("powerEig", "list")

  out
}
#' @noRd
#' @keywords internal
A_mult <- function(D, x)
{
  if (inherits(D, "dist")) {
    A_mult_dist(unclass(D), x)
  } else {
    drop(D %*% x)
  }
}


