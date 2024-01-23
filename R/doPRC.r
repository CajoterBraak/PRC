#' @title Perform a Principal Response Curve analysis (PRC)
#'
#' @description
#' \code{doPRC} perform a Principal Response Curve analysis (PRC) using the
#' \code{S3 formula} interface of \code{\link[vegan]{rda}} or \code{\link[vegan]{cca}}
#' @param formula formula specifying how the response (species data) must modeled by predictors and covariates.
#' A typical \code{formula} is \code{Y~treatment*week+Condition(week)},
#' where \code{Y} is  the global response data matrix or data frame.
#' Typically these are community (species) data. If count-like, they probably should be log transformed prior to the analysis,
#' because a multiplicative model has more realism for non-negative data than an additive model.
#' @param scale Scales species to unit variance (like correlations) if \code{method} is \code{"rda"}
#' (default: \code{FALSE}), else void. See \code{\link[vegan]{rda}}.
#' @param referencelevel  numeric or  character level(s) to be used as reference level or levels
#' of the focal treatment(s); default: 1 (first level(s)).  The focal treatment is (or treatments are) given in the
#' resulting list at \code{result$focal_and_conditioning_factors$`focal factor`}
#' @param data  Data frame containing the variables on the right hand side of the model formula.
#' @inheritParams PRC_scores
#' @details
#' The function performs PRC using \code{\link[vegan]{rda}} or \code{\link[vegan]{cca}} with
#' the specified \code{formula} and applies \code{\link{PRC_scores}} to the result.
#'
#'
#' @return an object as a \code{\link[vegan]{cca.object}} with additional entries as output of \code{\link{PRC_scores}}.
#' The assigned classes are \code{c("PRC","rda","cca"}.
#' @references
#' ter Braak (2023) Redundancy analysis includes analysis of variance-simultaneous component analysis (ASCA)
#'  and outperforms its extensions
#' Chemometrics and Intelligent Laboratory Systems https://doi.org/10.1016/j.chemolab.2023.104898
#'
#' ter Braak, C.J.F. & te Beest, D.E. 2022. Testing environmental effects on taxonomic composition
#'  with canonical correspondence analysis: alternative permutation tests are not equal.
#'  Environmental and Ecological Statistics. 29 (4), 849-868.
#'  https://doi.org/10.1007/s10651-022-00545-4
#'
#' van den Brink, P.J. & ter Braak, C. (1998) Multivariate analysis of stress in experimental ecosystems
#'  by Principal Response Curves and similarity analysis. Aquatic Ecology, 32, 163-178.
#'  http://dx.doi.org/10.1023/A:1009944004756
#'
#' van den Brink, P.J. & ter Braak, C.J.F. (1999) Principal Response Curves: Analysis of
#'  time-dependent multivariate responses of a biological community to stress.
#'  Environmental Toxicology and Chemistry, 18, 138-148.
#'  https://doi.org/10.1002/etc.5620180207
#'
#' van den Brink, P.J., den Besten, P., bij de Vaate, A. & ter Braak, C.J.F. (2009)
#'  Principal response curves technique for the analysis of multivariate biomonitoring time series.
#'  Environmental Monitoring and Assessment, 152, 271-281.
#'  http://dx.doi.org/10.1007/s10661-008-0314-6
#' @example demo/PRC_pyrifos.r
#' @export
doPRC <- function(formula, scale =  FALSE,  referencelevel = NULL, rank = 2,
                 flip = rep(FALSE,rank), scaling = "ms",  method = "rda",
                 data, ...){
  ev <- environment()
  parent.env(ev) <- environment(formula)
  environment(formula) <- ev
  if (method=="rda")  {
    mod_rda <- vegan:::rda.formula( formula= formula,  data = data, scale= scale,...)
  } else      mod_rda <- vegan:::cca.formula( formula= formula,  data = data,...)

  Design_w_PRCs <- PRC_scores(object = mod_rda,
                    focal_factor_name = NULL, referencelevel = referencelevel,
                    rank = rank, scaling = scaling,flip = flip, data = data)
  Design_w_PRCs$terms <- NULL;   Design_w_PRCs$terminfo <- NULL; Design_w_PRCs$method <- NULL
  out <- c(Design_w_PRCs,mod_rda)

  class(out) <- c("PRC",class(mod_rda))
  return(out)
}
