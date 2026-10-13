#' @title Smooth PRC using LMMsolver (Boer, 2023)
#' @param response matrix or data frame of the abundance data or a distance
#' matrix or object of class \code{\link[stats:dist]{dist}}
#' The abundance data has dimension \emph{sites} x \emph{species}.
#' @param model a named list.
#' Usually the  result of \code{\link{set_smoothPRC_model}}
#' The model contains at least one element and one or
#' more optional elements used for fitting the model using the
#' functions \code{fit_full_model} and \code{fit_null_model}) (see Details):
#' \itemize{
#' \item \code{method}: character. Default: \code{"dbrda"} for
#' distance-based smooth PRC, when \code{response} is a distance-like object.
#' The default is  \code{"rda"} for rda-based smooth PRC, when \code{response}
#' is an abundance matrix, leading to
#' models of the principal components family (pca, princomp, rda, dc-pca).
#' Any other string then leads to CCA-based smooth PRC.
#' \item \code{data}: an object,
#' usually a data frame with the predictor data for rows (sites)
#' \item \code{formula}: one or more optional formulas specifying the model
#' of the predictor data (if any) to
#' be fitted to a response vector.
#' }
#'
#' @param options_iter list of five elements
#' \itemize{
#' \item \code{init}: initial values of sample scores if  \code{response}
#' is a distance and of species loadings otherwise, a vector of the appropriate
#' length or a matrix with axis-specific initial values in columns.
#' Default \code{NULL} for solver and model specific initialization.
#' Default \code{NULL}, resulting in a start for each eigenvector from the model
#' \code{Treatment*Time + Condition(Time)}.
#' \item \code{k}: number of cycles in block-power updating to  better separate
#' the first eigen vector from the second. Default \code{2}.
#' \item \code{mA}: number of vectors in Anderson acceleration. Default \code{4}.
#' \item \code{tol}: stopping criterion, tolarance. Default \code{1e-4}.
#' \item \code{maxiter}:maximum number of iterations. Default \code{20}.
#' }
#' @param formula_covariates one-sided formula with row covariates
#' (sample-specific predictors) that the model needs to adjust for.
#' Default \code{NULL}, resulting in \code{~1}.
#' The formula specifies what is called  the \code{Condition} in \code{vegan},
#' covariates in \code{Canoco} and covariables in ter Braak & Prentice (1988).
#' The covariates must be in \code{model$data}.
#' @param n_axes the number of axes to extract. Default \code{2}.
#' @param weights a list of user-defined weights for columns (species) first
#'  and, second, rows (sites) Default \code{NULL} for method-defined weights,
#'  e.g. row totals of \code{response} in CCA, and uniform weights in RDA.
#'  Zero weights are not allowed; near zero weigths may lead to instability;
#'   this is unchecked currently.
#'   Ignored when \code{response} is a distance.
#' @param n_species number of species in \code{response}. Default 1.
#' Ignored if \code{response} is an abundance matrix.
#' For testing purposes, to get same scaling irrespectively when \code{response}
#' is an abundance matrix or a Euclidean distance matrix
#' of that abundance matrix.
#' @details
#' An atypical aspect of the function is that the time and treatment factors
#' need to have the names \code{Time} and \code{Treatment} in \code{data}.
#'
#' Classical PRC is a reduced rank model of rank 1 (or 2) with model
#' \code{~Treatment*Time} with the \code{Time} factor is a concomitant variable.
#' In smoothPRC the factor \code{Time} is taken quantitatively and
#' the main effect
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
#' become P-splines (penalized B-splines).
#' For an initial vector \code{b} of species scores,
#' the unconstrained sites
#' are calculated, which are then used as response vector in
#' \code{\link[LMMsolver]{LMMsolve}} to give a constrained site scores
#' \code{x} by the P-spline model specified in \code{model}. This
#' is part of a power algorithm with block and Anderson acceleration to solve
#' the dominant eigen vector of the
#' underlying eigen problem while optimizing simultaneously the penalties
#' using \code{\link[LMMsolver]{LMMsolve}}.
#'
#' The \code{smoothPRC} algorithm uses one and two-sided power iterations.
#' It thus iterates through the transition formulas of the method with
#' (P-spline) regression fits.
#' It uses two acceleration methods:
#' blocking and Anderson iteration.
#'
#' The \code{response} is taken as a distance object if
#' it inherits class  \link[stats:dist]{dist} or if it a matrix-like object with
#' equal dimensions using \code{\link[=dim]{dim}} and all zero diagonal using
#' \code{sum(}\code{\link[=diag]{diag}}\code{(response))==0}.
#' Initialization of the power algorithm expands the
#'  \link[stats:dist]{dist} object in \code{response}
#' to a square symmetrix matrix.
#' This expansion can be avoided by explicit initialization
#' using \code{options_iter$init}.
#'
#' If \code{response} is a distance object and the maximum distance exceeds 4
#' then the distance is divided by n-1 with n the number of samples.
#' This choice is taken from vegan::dbrda and usually makes the eigenvalues
#' equal to those of vegan::rda if the distance is 'euclidean'.
#'
#'
#' @return A list.
#' \describe{
#' \item{eig}{eigenvalues.}
#' \item{B}{matrix with response variable (species) scores \code{B}'s
#' with \code{Fratio}'s with attached axis number, e.g. \code{B1}.}
#' \item{X}{matrix with the eigen vectors
#' (i.e. the constrained sample scores) in columns.}
#' \item{X_star}{matrix with the unconstrained sample scores.}
#' \item{PRC}{matrix with the PRC scores, X scores compared to the X-scores of
#' the reference level in \code{Treatment} in \code{data}}
#' \item{PRC_star}{matrix with the unconstrained PRC scores.}
#' \item{weights}{names list of columns weights and row weights. Names are
#' \code{K}, \code{R}, \code{sqrtK}, \code{sqrtR}. The first two are
#' column (species) and row (site)totals in with \code{method != "rda"}.
#' They not normalized; none sums to 1. All 1 in \code{method =="rda"}}.
#' \item{obj}{a named list of \code{\link[LMMsolver]{LMMsolve}} objects,
#'  one for each axis.}
#' \item{objH0}{a named list of \code{\link[LMMsolver]{LMMsolve}} objects
#'  of the null model, one for each axis.}
#' \item{iter}{the number of iterations of the accelerated power algorithm.}
#' \item{Y}{the response matrix.}
#' \item{model}{copy of the \code{model} argument to the
#' \code{powerSVD}.}
#' \item{options_iter}{copy of the \code{opions_iter} argument
#'  to the \code{smoothPRC}.}
#' \item{axes}{an internal matrix: Intercept, optionally covariates and
#'  previous axes.}
#' \item{n_covariates}{the number of user-defined row covariates using
#' \code{formula_covariates} with one for the \code{"(Intercept)"}.}
#' }
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
#' @importFrom stats var
#' @export
smoothPRC <- function(response, model,
                      options_iter = list(init =NULL,
                                          k = 2, mA = 4,
                                          tol = 1e-8, maxiter = 20),
                      n_axes = 2,
                      formula_covariates = NULL,
                      weights = NULL,
                      n_species = 1) {
  formula_covariates_columns = NULL
  options_fit <- list(
    maxIter = NULL, tol = NULL, warm_start = TRUE,
    rows = NULL,rows0 = NULL,
    columns = NULL, columns0 = NULL
  )
  #if (!spam::is.spam(response)) response <- as.matrix(response)
  # if(inherits(response, "matrix")) {
  #   d <- dim(response); dd <- diag(response)
  # } else {d <-c(1,2); dd <-0}
  # if (n == (1 + sqrt(1 + 8 * length(response))) / 2 ||
  #     (d[1]==d[2] && sum(dd)==0) )
   if(inherits(response,"dist")|| (diff(dim(response))==0)&&
               sum(diag(as.matrix(response)))==0){
    pwr <- powerEig(D = response, model = model,
                    options_iter = options_iter,
                    options_fit = options_fit ,
                    n_axes = n_axes,
                    formula_covariates = formula_covariates)
  } else { # method = "rda" or "cca"
    pwr <- powerSVD(Y = response, model = model,
                    options_iter = options_iter,
                    options_fit = options_fit ,
                    n_axes = n_axes,
                    formula_covariates_rows = formula_covariates,
                    formula_covariates_columns = formula_covariates_columns,
                    weights =weights)
    Fratio <-fFratios_Time_X(pwr)
    pwr$B<- cbind(pwr$B, Fratio)
  }

  PRC<-  PRC_star <- matrix(NA, nrow = nrow(pwr$X), ncol= n_axes)
  if(inherits(pwr,"powerEig")) {
    n1<- sqrt((nrow(pwr$X) - 1)/n_species)
    pwr$mult <- n1/sqrt(pwr$eig[1])
    pwr$X <- pwr$X*n1
    pwr$X_star <- pwr$X_star*n1
    for (ax in 1:n_axes){
      PRC[, ax] <-  get_PRC(pwr, dat0 = pwr$model$dat0, k = ax)/
        sqrt(pwr$eig[ax])
      PRC[, ax] <- PRC[, ax] * n1
      PRC_star[, ax] <- PRC[, ax] +
        (pwr$X_star[,ax]- pwr$X[,ax])
    }
  } else {
    for (ax in 1:n_axes){
      PRC[,ax] <-  get_PRC(pwr, dat0 = pwr$model$dat0, k = ax)/
        sqrt(ncol(pwr$Y))
      PRC_star[,ax] <- PRC[,ax] + (pwr$X_star[,ax]- pwr$X[,ax])
    }
  }
  colnames(PRC)<- paste0("PRC",1:n_axes)
  colnames(PRC_star)<- paste0("PRC_star",1:n_axes)
  rownames(PRC)<- rownames(PRC_star)<-rownames(pwr$X)
  pwr$PRC <- PRC
  pwr$PRC_star <- PRC_star
  class(pwr) <- c("smoothPRC",class(pwr))
  return(pwr)
}
#' @noRd
#' @keywords internal
fFratios_Time_X <- function(smooth_PRC){
  # initialize
  Z <- model.matrix(~ smooth_PRC$model$data$Time)
  # residualize X on Time
  qrT <- qr(Z)
  X0 <- qr.resid(qrT, smooth_PRC$X)
  qrx <- qr(X0) # QR of residualized X
  Q   <- qr.Q(qrx)
  # local function
  fseq <- function(y) {
    # sequential SS
    ## needs qrT and Q as above
    # X: matrix of orthogonal columns x1,...,xm
    # residualize y on Time
    y0 <- qr.resid(qrT, y)
    a  <- crossprod(Q, y0)
    ss <- cumsum(drop(a^2))
    rss0 <- sum(y0^2)
    n  <- length(y)
    pT <- qrT$rank
    k  <- seq_along(ss)
    Fratio <- (ss / k) / ((rss0 - ss) / (n - pT - k))
    return(Fratio)
  }
  Fratio <- t(apply(smooth_PRC$Y,2,fseq))
  if (nrow(Fratio)==1)Fratio<-t(Fratio)
  colnames(Fratio)<- paste0("Fratio", 1:ncol(Fratio))
  return(Fratio)
}
#' @noRd
#' @keywords internal
get_PRC <-function(object, dat0=NULL, k = 1){
  # full fitted values
  xhat <- fitted(object$obj[[k]])
  pred0 <- predict(object$obj[[k]] , newdata = dat0)
  PRC <- xhat - pred0$ypred # only the deviations from the control
  #xmean <-  - mean(xhat)
  return(PRC)
}
