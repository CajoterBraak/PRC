#' @title Taxon fit statistics of an rda or cca ordination
#'
#' @description
#' \code{rdacca_taxon_stats} calculates fit statistics for taxa in an \code{\link[vegan]{rda}}
#' or \code{\link[vegan]{cca}} ordination.
#'
#' @param  object  a result of \code{\link[vegan]{rda}} or \code{\link[vegan]{cca}}.
#' @param  rank number of axes for which fits are calculated (1:rank). Default: 1.
#' @details
#' The code uses \code{\link[vegan]{ordiYbar}} and  \code{\link[vegan]{scores}} and also works for unconstrained ordination
#' using \code{\link[vegan]{rda}}and \code{\link[vegan]{cca}}. The software Canoco 5 (ter Braak & Smilauer, 2018) contains Cfit and a rescaled
#' version of the column Var but not the columns Fratio.k.
#'
#' @return A matrix with taxa in rows and taxon specific statistics in columns:
#' \describe{
#' \item{Var}{sample variance of the taxon.}
#' \item{FitCE}{R squared: fraction of the sample variance explained
#' by the predictors and covariates.}
#' \item{FitE}{fraction of the sample variance explained by the
#' predictors after adjustment for the covariates.}
#' \item{Cfit\emph{k}}{fraction of the sample variance explained
#' by the first \emph{k} axes (the axes are constrained
#' in a constrained ordination and unconstrained otherwise).}
#' \item{Fratio\emph{k}}{F-ratio of the regression model with \emph{k} axes (as predictors) and the model without axes
#' (i.e. with covariates or the intercept only).}
#' }
#' @example demo/PRC_pyrifos_bk.R
#' @references
#' ter Braak C.J.F. & Šmilauer P. (2018): Canoco reference manual and user's guide:
#'  software for ordination, version 5.1x. Microcomputer Power, Ithaca, USA, 536 pp.
#'  (http::www.canoco5.com)
#' @export

rdacca_taxon_stats <- function(object, rank = 1){
  # object is a vegan cca-object using  cca, rda
  # output are the taxon-stats as in Canoco 5 (ter Braak & Smilauer 2018)
  # Canoco reference manual and user's guide: software for ordination, version 5.1x. Microcomputer Power, Ithaca, USA, 536 pp.
  #Ter Braak C.J.F. & Šmilauer P. (2018): Canoco reference manual and user's guide: software for ordination, version 5.1x. Microcomputer Power, Ithaca, USA, 536 pp.


  totspe = colSums(vegan::ordiYbar(object, model = "initial")^2) # different from varR in Canoco as that one is scaled.
  if (!is.null(object$CCA)) SS_CCA <- colSums(vegan::ordiYbar(object, model = "CCA")^2) else  SS_CCA <- 0

  FitE = SS_CCA/totspe # Fit_E in PRC canoco
  if (!is.null(object$pCCA)) {
    FitCE = (SS_CCA + colSums(vegan::ordiYbar(object, model = "pCCA")^2))/totspe
    q <-object$pCCA$QR$rank +1
  } else {FitCE = NULL; q <- 1}


  if (!is.null(object$CCA)) mymodel <- "CCA" else mymodel <- "CA"
  Cfit <- Fratio <- matrix(NA, nrow = nrow(vegan::scores(object, display="sp", choices = 1)),ncol = rank)

  for (k in 1:rank){
       sc <-vegan::scores(object, display=c("sp", "sites"), choices = 1:k , scaling = 1)
       if (object$method=="rda") SS_CCA_rank <- colSums((sc$sites %*% t(sc$species))^2)/(nobs(object)-1) else {
         SS_CCA_rank <- colSums(diag(object$rowsum) %*% (sc$sites %*% t(sc$species))^2 %*% diag(object$colsum))
       }
    Cfit[,k] <- SS_CCA_rank/totspe
      ResidualSS_of_rank_k <- SS_CCA + colSums(vegan::ordiYbar(object, model = "CA")^2) - SS_CCA_rank
      ResidualMS_of_rank_k <- ResidualSS_of_rank_k/(nobs(object) - q - k)
      Fratio[,k] <- (SS_CCA_rank/k)/ ResidualMS_of_rank_k

  }
  colnames(Cfit) <- paste("Cfit", 1:ncol(Cfit),sep= "")
  colnames(Fratio) <- paste("Fratio", 1:ncol(Fratio),sep= "")
  out <- cbind(Var = totspe, FitCE, FitE,  Cfit, Fratio)
  return(out)
}

