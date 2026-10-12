#' @title Power algorithm for eigen analysis of a model for a distance matrix
#' @param D symmetric distancce matrix (internally squared!)
#' @param model a named list, containing at least one element and one or
#' more optional elements used for fitting the model using the
#' functions \code{fit_full_model} and \code{fit_null_model}) (see Details):
#' \itemize{
#' \item \code{method}: character. Default: \code{"rda"} for
#' models of the principal components family (pca, princomp, rda, dc-pca).
#' Any other string leads to the the correspondence analysis family
#'  (ca, cca, dc-CA).
#' \item \code{data_rows}: an optional object,
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
#' \code{powerEig}.
#' @param options_iter list of five elements
#' \itemize{
#' \item \code{init}: initial values of the sample (site) scores (a vector or
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
#' @details
#' Two additional functions must be supplied
#' that specify a model for a univariate response vector,
#' a weighted sum or weighted average of
#' the columns of matrix \code{D}
#' for \code{method =="rda"} and \code{method ="cca"}, respectively.
#' The functions are:
#' \itemize{
#' \item \code{fit_full_model_rows}: a function of three arguments
#' \code{model}, \code{y} and \code{weights}, resulting in
#' a fitted object with the function \code{fitted()}, resulting in
#' fitted values for \code{y} based on the model (the  full model H1).
#' The weights can be \code{NULL} for method-defined weights only,
#' \emph{e.g.} uniform weights for PCA/RDA-type methods and weights based on the
#' row and column sums of \code{D} for CA/CCA-type methods.
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
#' The \code{powerEig} algorithm implements two-sided power iteration.
#' It thus iterates through  the transition formulas of the method.
#' It uses two acceleration methods:
#' blocking and Anderson iteration.
#'
#' Beyond algorithm differences, \code{powerEig} differs from standard
#'  \code{\link{svd}} in at least two aspects. First,
#' \code{powerEig} will always implicitly column-center the data \code{D},
#' corresponding to a centered PCA/RDA and to standard CA/CCA. With
#' column models beyond the saturated or identity model all methods will
#' also implicitly row-center the data \code{D}. Without that, the method would
#' be scale-dependent, \emph{i.e.} depend on the chosen scale
#' of column predictors, which nearly always completely arbitrary.  Second, the
#' result is not a list with the standard elements of the result of
#' \code{\link{svd}}. The role of \code{u,v,d} is taken by \code{X,B, eig}, with
#' eig the square of the singular values \code{d}.
#'
#' Specifically, the columns of \code{X} for rows of \code{D}
#' and \code{B} for columns of \code{D} are eigenvectors/singular vector, but
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
#' The value is \code{sqrt(ncol(D))} giving a mean square of 1
#' of the species scores in RDA. The value is 1 in CCA.}
#' \item{obj}{a named list of \code{\link[LMMsolver]{LMMsolve}} objects,
#'  one for each axis.}
#' \item{objH0}{a named list of \code{\link[LMMsolver]{LMMsolve}} objects
#'  of the null model, one for each axis.}
#' \item{iter}{the number of iterations of the accelerated power algorithm.}
#' \item{D}{the response matrix.}
#' \item{model}{copy of the \code{model} argument to the
#' \code{powerEig}.}
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
powerEig <- function(D, model,
                        options_iter = list(init =NULL,
                                            k = 2, mA = 4,
                                            tol = 1e-10, maxiter = 30),
                        options_fit = list(
                         maxIter = NULL, tol = NULL, warm_start = TRUE,
                         rows = NULL,rows0 = NULL,
                         columns = NULL, columns0 = NULL
                       ),
                      n_axes = 2,
                      formula_covariates = NULL,
                      formula_covariates_columns = NULL) {

  model$method <- "dbrda"
  if(max(D)>= 4 + .Machine$double.eps) {
    mult <- if(is.matrix(D))
      sqrt(nrow(D)-1) else sqrt((1 + sqrt(1 + 8 * length(D))) / 2 - 1 )
    D <- D/mult
  }
  weights <- NULL
  if (is.null(options_fit)) options_fit = list(
    maxIter = NULL, tol = NULL, warm_start = TRUE,
    rows = NULL,rows0 = NULL,
    columns = NULL, columns0 = NULL
  )
   if (is.null(formula_covariates)) formula_covariates <- ~1
   covariate_data <- model.matrix(formula_covariates,
                                  data = model$data)
   n_covariates <- ncol(covariate_data)
   axes<- covariate_data
   if (!is.null(formula_covariates_columns)){
     qr_covariates_columns <- qr(model.matrix(formula_covariates_columns,
                                data = model$data_columns))
   } else qr_covariates_columns <- NULL
# double center D^2 (A <- -0.5 * double_center(D^2)) not needed
# as the centring of z, even with the null covariates, implies the centring.
   A <- -0.5 * D^2
  if (inherits(D, "dist")) {
   A <- D
   rownams <- attr(A,"Labels")
   A[] <- -0.5*A[]^2
  } else {
    A <- -0.5 * D^2
    rownams <- rownames(D)
  }
   pwrEig1 <- powerEig_axis(A            = A,
                            model        = model,
                            options_iter = options_iter,
                            fit_state    = options_fit,
                            weights      = weights,
                            axes         = axes,
                            n_covariates =  n_covariates)

    iaxis = 1

    while(iaxis < n_axes && pwrEig1$eig[iaxis] > 1.0e-2*pwrEig1$eig[1]){
      pwrEig1$fit_state$rows <- pwrEig1$fit_state$rows0 <-
      pwrEig1$fit_state$columns <- pwrEig1$fit_state$columns0 <- NULL
      pwrEig2 <- powerEig2(  pwrEig_axis = pwrEig1,
                             options_iter =  options_iter,
                             fit_state = pwrEig1$fit_state)
      pwrEig1 <- pwrEig2
      iaxis <- iaxis+1
    }
    #pwrEig1$eig <- ncol(A)* apply(pwrEig1$X,2, var)
    names(pwrEig1$obj) <-names(pwrEig1$objH0) <-
      paste0("axis",seq_along(pwrEig1$obj))
    rownames(pwrEig1$X)<-rownames(pwrEig1$X_star) <- rownams

    # todo Add species scores???
  return(pwrEig1)
}

