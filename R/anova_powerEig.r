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
#' @noRd
anova_powerEig <- function(object,
                            permutations = 19, verbose = TRUE) {
  #object from smoothPRC using powerSVD
  axes <- object$axes[,1:object$n_covariates,drop = FALSE] # all covariates with intercept
  N <- nrow(object$X)
  weights <-list(sqrtR = rep(1,N), R = rep(1,N))
  qr_prev_axes <- qr(axes)
  Fstat0 <- test_stat_prc1(object, weights = weights, ax =1)
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

  #cov_nams<- covariate_names(object$model$object$fixedH0)
  #adjust_for_covariates <- length(cov_nams)
  #adjust_for_covariates <- FALSE # do Manly, permutation of raw data with F stat
  EPS <- sqrt(.Machine$double.eps) # for permutation P-values
  Fstat <- numeric(nperm)
  iter <- numeric(nperm)
  outii.list<- vector(mode = "list", length = nperm)
  for (ii in 1:nperm){
    if (verbose && !(ii%%10))cat("iteration",ii, "out of ", nperm, "\n")
    permii <- perm.mat[ii,]

    Aii <-if(is.matrix(object$A) ) object$A[permii,permii] else
      permute_dist(object$A, permii)
    # if(adjust_for_covariates){
    #  object$model$data[,cov_nams] <-  object$model$data[permii, cov_nams]
    #  object$model$dat0[,cov_nams] <-  object$model$dat0[permii, cov_nams]
    # }
    outii <- powerEig_axis(Aii, model= object$model,
                            options_iter = object$options_iter,
                           weights = object$weights,
                           n_covariates = object$n_covariates,
                            axes = NULL,
                           qr_prev_axes = qr_prev_axes
                        )
    outii.list[[ii]]<- outii
    iter[ii] <- outii$iter
    if(is.null(outii$weights)) {
      outii$weights <-list(sqrtR = rep(1,N), R = rep(1,N))
    }
    Fstat[ii]<-test_stat_prc1(outii, weights=outii$weights, ax = 1)
  }
  isna.r <- sum(is.na(Fstat))

  pval <- (sum(Fstat > (Fstat0 + EPS), na.rm = TRUE) + 1)  / (nperm- isna.r  + 1)
  ret <- list(pval = pval, F0 = Fstat0, Fstat = Fstat, iter = iter,
              outii.list= outii.list)
  if (verbose) print(pval)
  return(ret)
}
permute_dist <- function(D, perm)
{
  out <- permute_dist_cpp(
    unclass(D),
    as.integer(perm),
    length(perm)
  )
  attributes(out) <- attributes(D)
  return(out)
}
