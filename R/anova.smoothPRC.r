#' @title Permutation Test for Smooth PRC

#' @param object an object from \code{\link{smoothPRC}}.
#' @param permutations a list of control values for the permutations as
#' returned by the function \code{\link[permute]{how}}, or the number of
#' permutations required (default 19) or a permutation matrix where each row
#' gives the permuted indices.
#' @param verbose logical for printing progress of the permutations
#' @param ... unused.
#' @details
#' The test takes account of additional covariates in \code{fixed} of
#' the LLMsolver model. The test is approximate without permutation of whole
#' time series.
#' The test is based on the first axis.
#' @export
anova.smoothPRC <- function(object,
                            ...,
                            permutations = 19, verbose = TRUE) {
  #object from smoothPRC
  axes <- object$axes[,1:object$n_,drop = FALSE] # all covariates with intercept
  qr_prev_axes <- qr(axes*object$weights$sqrtR)
  Fstat0 <- test_stat_prc(object)
  # code
  if (inherits(permutations, c("numeric", "how", "matrix"))) {
    if (is.numeric(permutations) && !is.matrix(permutations)) {
      permutations <- permute::how(nperm = permutations[1])
    } else if (is.matrix(permutations) && ncol(permutations) != N) {
      stop("Each row of permutations should have", N, "elements.\n")
    }
  } else {
    stop("Argument permutations should be integer, matrix ",
         "or specified by permute::how().\n")
  }

  N <- nrow(object$Y)
  if (is.matrix(permutations)) {
    # matrix: check that it *strictly* integer
    if (!is.integer(permutations) && !all(permutations == round(permutations))){
      stop("Permutation matrix must be strictly integers: use round().\n")
    }
    perm.mat <- permutations
  } else if (inherits(permutations, "how")){
    #perm.mat creation
    perm.mat <- permute::shuffleSet(N, control = permutations)
  }
  nperm <- nrow(perm.mat)

  #cov_nams<- covariate_names(object$lmm_model$object$fixedH0)
  #adjust_for_covariates <- length(cov_nams)
  #adjust_for_covariates <- FALSE # do Manly, permutation of raw data with F stat
  EPS <- sqrt(.Machine$double.eps) # for permutation P-values
  Fstat <- numeric(nperm)
  iter <- numeric(nperm)
  objH0.list <- vector(mode = "list", length = nperm)
  objH1.list <- vector(mode = "list", length = nperm)
  for (ii in 1:nperm){
    if (verbose && !(ii%%10))cat("iteration",ii, "out of ", nperm, "\n")
    permii <- perm.mat[ii,]
    Yii <- object$Y[permii,]
    # if(adjust_for_covariates){
    #  object$lmm_mode$data[,cov_nams] <-  object$lmm_mode$data[permii, cov_nams]
    #  object$lmm_mode$dat0[,cov_nams] <-  object$lmm_mode$dat0[permii, cov_nams]
    # }
    outii <- smoothPRC1(Yii, lmm_model= object$lmm_model,
                            options_iter = object$options_iter,
                            weights = object$weights,
                            axes = NULL,
                            qr_prev_axes = qr_prev_axes
                        )
    objH1.list[[ii]]<- outii$obj
    objH0.list[[ii]]<- outii$objH0
    iter[ii] <- outii$iter
    Fstat[ii]<-test_stat_prc(outii)
  }
  isna.r <- sum(is.na(Fstat))

  pval <- (sum(Fstat > (Fstat0 + EPS), na.rm = TRUE) + 1)  / (nperm- isna.r  + 1)
  ret <- list(pval = pval, F0 = Fstat0, Fstat = Fstat, iter = iter,
              objH1.list= objH1.list, objH0.list= objH0.list )
  if (verbose) print(pval)
  invisible(ret)
}
# covariate_names <- function(formula) {
#   all.vars(delete.response(terms(formula)))
# }
#' @noRd
#' @keywords internal
test_stat_prc <- function(out, ax = 1){
  df.resH1<-  fEDdf_res(out$obj[[ax]])
  #SSnum <-sum(fit_H1^2) - sum(fit_H0^2)
  # RSS.H0- RSS.H1
  RSS.H1 <- sum(out$weights$R*(out$YB[,ax] - fitted(out$obj[[ax]]))^2)
  SSnum <- sum(out$weights$R*(out$YB[,ax]- fitted(out$objH0[[ax]]))^2) - RSS.H1
  MSnum <- SSnum/(fEDdf_res(out$objH0[[ax]])-df.resH1)
  MSden <- RSS.H1 /df.resH1
  Fratio <- MSnum/MSden
  #Fratio <- -SSnum
  return(Fratio)
}
#' @noRd
#' @keywords internal
fEDdf_res <- function(out_obj){
  # calculates the effective residual degrees of freedom from a LMMsolveObject
  dfs <-out_obj$EDdf
  df_res <- dfs[nrow(dfs),2]
  return(df_res = df_res)
}
