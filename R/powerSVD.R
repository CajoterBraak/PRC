#' @title Generic two-sided power algorithm for SVD of data models
#' @param Y matrix or data frame of the abundance data
#' (dimension \emph{sites} x \emph{species})
#' @param model a named list, containing at least one element and one or
#' more optional elements used for fitting the model using the
#' functions \code{fit_full_model} and \code{fit_null_model}) (see Details):
#' \itemize{
#' \item \code{method}: character. Default: \code{"rda"} for
#' models of the principal components family (pca, princomp, rda, dc-pca).
#' Any other string leads to the the correspondence analysis family
#'  (ca, cca, dc-CA).
#' \item \code{data}: an optional object,
#' usually a data frame with the predictor data for rows (sites),
#' used on the basis of \code{formula_covariates_rows}.
#' \item \code{data_columns}: an optional object,
#' usually a data frame with the predictor data for columns (species),
#' used on the basis of \code{formula_covariates_columns}.
#' \item \code{formula}: one or more optional formulas specifying the model
#' of the predictor data (if any) to
#' be fitted to a response vector.
#' }
#' Currently result of \code{\link{set_smoothPRC_model}} for smooth PRC using
#' \code{powerSVD}.
#' @param options_iter list of five elements
#' \itemize{
#' \item \code{init}: initial values of the species loadings (a vector or
#' matrix with axis-specific initial values in columns.
#' Default \code{NULL} for solver and model specific initialization.
#' \item \code{k}: number of cycles in block-power updating to  better separate
#' the first eigen vector from the second. Default \code{NULL}.
#' \item \code{mA}: number of vectors in Anderson acceleration. Default \code{3}.
#' \item \code{tol}: stopping criterion, tolarance. Default \code{1e-8}.
#' \item \code{maxiter}:maximum number of iterations. Default \code{50}.
#' }
#' @param formula_covariates_rows one-sided formula with row covariates
#' (sample-specific predictors) that the model needs to adjust for.
#' Default \code{NULL}, resulting in \code{~1}.
#' The formula specifies what is called  the \code{Condition}in \code{vegan},
#' covariates in \code{Canoco} and covariables in ter Braak & Prentice (1988).
#' @param formula_covariates_columns one-sided formula with column covariates
#'(column-specific predictors) that the model needs to adjust for.
#' Default \code{NULL}, resulting in \code{~1}, if there is
#' a constrained column model and no effect otherwise.
#' The formula specifies what is called  the \code{Condition}in \code{vegan},
#' covariates in \code{Canoco} and covariables in ter Braak & Prentice (1988).
#' @param n_axes the number of axes to extract. Default \code{2}.
#' @param weights a list of user-defined weights for columns (species) first
#'  and, second, rows (sites) Default \code{NULL} for method-defined weights,
#'  e.g. row totals of \code{Y} in CCA, and uniform weights in RDA.
#'  Zero weights are not allowed; near zero weigths may lead to instability;
#'   this is unchecked currently.
#' @details
#' Two additional functions must be supplied
#' that specify a model for a univariate response vector,
#' a weighted sum or weighted average of
#' the columns of matrix \code{Y}
#' for \code{method =="rda"} and \code{method ="cca"}, respectively.
#' The functions are:
#' \itemize{
#' \item \code{fit_full_model_rows}: a function of three arguments
#' \code{model}, \code{y} and \code{weights}, resulting in
#' a fitted object with the function \code{fitted()}, resulting in
#' fitted values for \code{y} based on the model (the  full model H1).
#' The weights can be \code{NULL} for method-defined weights only,
#' \emph{e.g.} uniform weights for PCA/RDA-type methods and weights based on the
#' row and column sums of \code{Y} for CA/CCA-type methods.
#' \item \code{fit_null_model_rows}: a function of three arguments
#' \code{model}, \code{xstar} and \code{weights}, resulting in
#' a fitted object with values for \code{fitted()}, fitted values
#' of the null model; the return value of \code{fit_null_model_rows}
#'  can be \code{NULL} if the null model contains
#'  the intercept only or just fixed effects of covariates specified
#'  by \code{formula_covariates_rows}.The weights can be \code{NULL} for
#' method-defined weights only.
#'  }
#'
#' The \code{powerSVD} algorithm implements two-sided power iteration.
#' It thus iterates through  the transition formulas of the method.
#' It uses two acceleration methods:
#' blocking and Anderson iteration.
#'
#' Beyond algorithm differences, \code{powerSVD} differs from standard
#'  \code{\link{svd}} in at least two aspects. First,
#' \code{powerSVD} will always implicitly column-center the data \code{Y},
#' corresponding to a centered PCA/RDA and to standard CA/CCA. With
#' column models beyond the saturated or identity model all methods will
#' also implicitly row-center the data \code{Y}. Without that, the method would
#' be scale-dependent, \emph{i.e.} depend on the chosen scale
#' of column predictors, which nearly always completely arbitrary.  Second, the
#' result is not a list with the standard elements of the result of
#' \code{\link{svd}}. The role of \code{u,v,d} is taken by \code{X,B, eig}, with
#' eig the square of the singular values \code{d}.
#'
#' Specifically, the columns of \code{X} for rows of \code{Y}
#' and \code{B} for columns of \code{Y} are eigenvectors/singular vector, but
#' not normalized as in standard \code{\link{svd}}. Instead, the mean square of
#' columns of \code{B} are set equal to one and thore for columns of \code{X}
#' are proportional to the eigenvalues (and adhere to the transition formulas).
#'  In this scaling, \code{B} and \code{X} form a row-metric preserving biplot.
#'
#' @return A list.
#' \describe{
#' \item{eig}{eigenvalues.}
#' \item{B}{matrix with response variable (species) scores \code{B}'s
#' with \code{Fratio}'s with attached axis number, e.g. \code{B1}.}
#' \item{X}{matrix with the eigen vectors
#' (i.e. the constrained sample scores) in columns.}
#' \item{X_star}{matrix with the unconstrained sample scores.}
#' \item{mult}{multiplier for re-scaling the species and sample scores.
#' The value is \code{sqrt(ncol(Y))} giving a mean square of 1
#' of the species scores in RDA. The value is 1 in CCA.}
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
#' \code{formula_covariates_rows} with one for the \code{"(Intercept)"}.}
#' }
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
#' @importFrom stats var
#' @noRd
#' @keywords internal
powerSVD <- function(Y, model,
                        options_iter = list(init =NULL,
                                          k = 2, mA = 4,
                                          tol = 1e-3, maxiter = 5),
                        options_fit = list(
                         maxIter = NULL, tol = NULL, warm_start = TRUE,
                         rows = NULL,rows0 = NULL,
                         columns = NULL, columns0 = NULL
                       ),
                      n_axes = 2,
                      formula_covariates_rows = NULL,
                      formula_covariates_columns = NULL,

                      weights = NULL) {
  if (!spam::is.spam(Y)) Y <- as.matrix(Y)
  if (is.null(options_fit)) options_fit = list(
    maxIter = NULL, tol = NULL, warm_start = TRUE,
    rows = NULL,rows0 = NULL,
    columns = NULL, columns0 = NULL
  )
   if (is.null(formula_covariates_rows)) formula_covariates_rows <- ~1
   covariate_data <- model.matrix(formula_covariates_rows,
                                  data = model$data)
   n_covariates <- ncol(covariate_data)
   axes<- covariate_data # cbind(covariate_data ,axes)
   # print("axes")
   # print(axes)
  # print("end axes")
   if (!is.null(formula_covariates_columns)){
     qr_covariates_columns <- qr(model.matrix(formula_covariates_columns,
                                data = model$data_columns))
   } else qr_covariates_columns <- NULL
   if (!is.null(weights)){
     if (length(weights)<2)stop("ERROR: weights must be a list of species and",
    "site weigths")
     for (k in 1:2){
       if (!is.null(weights[[k]])) {
         if (!length(weights[[k]])==c(rev(dim(Y)))[k]) stop(
           c("species", "sites")[k], "-weights must",
         " be of the same length as the number of ",  c("species", "sites")[k],
         ".\nwhich is ", c(rev(dim(Y)))[k],
         ".\n Note that, in the weights list, species weights come first,",
         " site weights second.")
      Y<- if (k==1) {
          Y * matrix(weights[[1]], nrow = nrow(Y), ncol= ncol(Y), byrow = TRUE)
          } else {
              Y*weights[[2]]
          }

       }
     }
   }
   #print(weights)
   weights <- set_weights(Y, model$method, user_weights = weights)
   #print(weights)
   pwrSVD1 <- powerSVD_axis(Y            = Y,
                            model    = model,
                            options_iter = options_iter,
                            fit_state = options_fit,
                            weights = weights,
                            axes         = axes,
                            qr_covariates_columns = qr_covariates_columns,
                            n_covariates =  n_covariates)

    iaxis = 1

    while(iaxis < n_axes && pwrSVD1$eig[iaxis] > 1.0e-2*pwrSVD1$eig[1]){
      pwrSVD1$fit_state$rows <- pwrSVD1$fit_state$rows0 <-
      pwrSVD1$fit_state$columns <- pwrSVD1$fit_state$columns0 <- NULL
      pwrSVD2 <- powerSVD2(pwrSVD_axis = pwrSVD1,
                             options_iter =  options_iter,
                             fit_state = pwrSVD1$fit_state)
      pwrSVD1 <- pwrSVD2
      iaxis <- iaxis+1
    }
    #pwrSVD1$eig <- ncol(Y)* apply(pwrSVD1$X,2, var)
    names(pwrSVD1$obj) <-names(pwrSVD1$objH0) <-
      paste0("axis",seq_along(pwrSVD1$obj))
    # todo Fratio. Currently smoothPRC specific!
    # if(pwrSVD1$model$method == "rda"){
    #   pwrSVD1$B <- cbind(pwrSVD1$B,fFratios(pwrSVD1))
    # }
    rownames(pwrSVD1$X)<-rownames(pwrSVD1$X_star) <- rownames(Y)
  return(pwrSVD1)
}
#' @noRd
#' @keywords internal
#'
set_weights <- function(Y, method, user_weights = NULL){
  if (method =="rda") {
    # rda
    if(is.null(user_weights)){
    n <- nrow(Y); m <- ncol(Y)
    R <-rep(1, n); K <-  rep(1, m)
    sqrtR = R
    #rep(sqrt(1/n), n);
    sqrtK=  K
    #rep(sqrt(1/m), m)
    } else {
      K <- user_weights[[1]]
      R <- user_weights[[2]]
      sqrtK <- sqrt(K* (length(K)/sum(K)))
      sqrtR <- sqrt(R* (length(R)/sum(R)))
    }
  } else {
    # cca
    R <-  rowSums(Y); K <- colSums(Y)
    sumY <- sum(R);
    sqrtR <- sqrt(R/sumY); sqrtK<- sqrt(K/sumY)
  }
  weights = list(K = K, R = R, sqrtK = sqrtK, sqrtR = sqrtR)
  return(weights)
}

