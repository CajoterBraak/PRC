#' @title Calculates principal response curve (PRC) scores from vegan::rda, cca, dbrda or capscale
#'
#' @description
#' \code{PRC_scores} calculates scores to display principal response curves with
#' scores for individual plots from a vegan constrained ordination object (\code{\link[vegan]{cca.object}}).
#'
#' @param  object  a \code{vegan} \code{\link[vegan]{cca.object}},
#'  a result of \code{\link[vegan]{rda}}, \code{\link[vegan]{cca}},
#' \code{\link[vegan]{dbrda}} or \code{\link[vegan]{capscale}}.

#' @param  focal_factor_name name of the focal factor (the treatment of interest)
#' @param referencelevel     numeric level or character (reference level of the focal factor)
#' @param rank               number of axes to be computed
#' @param flip               logical value or vector. Should the axes be flipped,
#' i.e. reversed in orientation? \code{flip} can be numeric with values -1 and 1
#' for reverse and no reverse, respectively,
#' i.e. multiply the scores by either -1 or 1.
#' @param scaling    character ("ms", "ss"). Scales the axes of an RDA object to mean square species scores of 1 ("ms") or
#'   a sum of squares of 1 ("ss"). The only scaling for a CCA  object is "ss" (weighted mean square = 1, with weigth =
#'   species total abundance). For a dbrda object, there are no species scores and the scaling is like "ss".
#'  The default scaling ("ms" for RDA and "ss" for CCA) has the advantage that
#'                    the PRC treatment and sample scores measure the effect sizes of the 'average' species
#'                    as their (weigthed) mean square is 1 (sd = 1); it is the scaling used in Canoco5 (www.canoco5.com).
#'                    Scaling "ss" is the default in  \code{\link[stats]{prcomp}}
#'                    and used in ter Braak 2023
#' @param data data frame of the experimental design used in the call
#'  to \code{\link[vegan]{rda}} or \code{\link[vegan]{cca}}
#' @return  A named list:
#' \item{PRCplus}{a data frame that combines the design in \code{data} with the PRC and RDA/CCA scores
#'                    with ("_E") and without error (the unconstrained [sites] and constrained [LC] scores, respectively),
#'                    and, finally, the reference scores. Note that
#'                    PRC1_E = RDA1_E - RDA1_Ref, etc for other axes}
#' \item{species}{a matrix with the loadings with respect to the PRC scores; NULL if the \code{object} lacks loadings.}
#' \item{reference_scores}{a matrix with the scores of each unit in \code{data} with the level of the focal factor
#'  replaced by the reference level}
#' \item{focal_factor_name}{name of the focal factor in the call to \code{PRC_scores}}
#' \item{referencelevel}{referencelevel in the call to \code{PRC_scores}}
#' @details \code{PRC_scores} uses \code{\link[vegan]{scores}} to
#' calculate the loadings (species scores),if obtainable from the \code{object}, and the constrained and unconstrained sample scores
#' of the units in \code{data}. The PRC treatment and sample scores are obtained by subtracting the reference scores
#' from the constrained and unconstrained scores, respectively.
#' The reference scores are calculated by applying \code{\link[stats]{predict}}
#' with \code{type = "lc"} on \code{newdata}, which
#' equals \code{data}, except that the level of the focal factor of each unit is changed to the reference level.
#' By this simple method, \code{prc scores} can be applied with and without terms in \code{Condition()}
#' in the \code{formula} of the call to \code{\link[vegan]{rda}}, \code{\link[vegan]{cca}},
#' \code{\link[vegan]{dbrda}} or \code{\link[vegan]{capscale}}.
#' There can thus be additional (perhaps quantitative) covariates,
#'  \code{A:B+ Condition(A+other_covariates)} where the focal factor is B,
#'  and also with three (instead of two factors) e.g.
#'  with formula \code{ A*B*C + Condition(A*C)} so as to display the A*C-dependent effects of focal factor B
#'  on the multivariate response in the call to \code{\link[vegan]{rda}}, \code{\link[vegan]{cca}},
#' \code{\link[vegan]{dbrda}} or \code{\link[vegan]{capscale}}.
#'
#'  CCA-based PRC is useful in data with many zeroes and which are strictly compositional,
#'  i.e. have row sums that have no meaning for the question of interest (ter Braak and te Beest, 2022).
#'  A layman's example is that
#'  the weight of a cake is not important for its taste; taste depends on its composition only.
#'  CCA can be applied without log-transforming the data and thereby avoids the issue of
#'  replacement of zeroes (ter Braak and te Beest, 2022).
#'
#' @example demo/PRC_Ossenkampen.r
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
#'  by Principal Response Curves and similarity analysis.
#'  Aquatic Ecology, 32, 163-178.
#'  http://dx.doi.org/10.1023/A:1009944004756

#' van den Brink, P.J. & ter Braak, C.J.F. (1999) Principal Response Curves: Analysis of
#'  time-dependent multivariate responses of a biological community to stress.
#'  Environmental Toxicology and Chemistry, 18, 138-148.
#'  https://doi.org/10.1002/etc.5620180207
#'
#' van den Brink, P.J., den Besten, P., bij de Vaate, A. & ter Braak, C.J.F. (2009)
#'  Principal response curves technique for the analysis of multivariate biomonitoring time series.
#'  Environmental Monitoring and Assessment, 152, 271-281.
#'  http://dx.doi.org/10.1007/s10661-008-0314-6
#'



#' @export

PRC_scores <-  function(object, focal_factor_name, referencelevel = 1, rank = 2, flip = rep(FALSE,rank),
                        scaling = "ms",data ){

   if (scaling == "sym") stop("scaling = \"sim\" has not yet been implemented")
   if (object$method == "cca") scaling <- "ss"
   if(!focal_factor_name %in% names(object$terminfo$xlev)) stop("Error in PRC_scores:focal_factor_name not in the model")

   myconst <- sqrt((nobs(object)-1)*object$tot.chi) # scaling of scores and loadings as in prcomp (and this paper).
   #if (scaling == "vegan::prc") myconst = (nobs(object)-1)*object$CCA$eig^0.25
   rank <- min(rank,length(object$CCA$eig))


   sc <- vegan::scores(object, display=c("sites","lc"), choices = 1:rank , scaling = 1, const = myconst)

   if (is.logical(flip)) flip <- ifelse(flip, -1,1)
   if (length(flip)!=rank){
     if (length(flip)>rank) flip = flip[1:rank]  else  flip <- c(flip, rep(1, rank-length(flip)))
   }
   if (length(flip)==1) flip = matrix(flip,nrow = 1, ncol=1 ) else flip = diag(flip)



   DesignRef <- data

   if (is.null(levels(DesignRef[[focal_factor_name]]))) print(errorCondition("Error in PRC_scores:focal_factor_name or reference erroneous"))

   if (is.numeric(referencelevel))  Bref <- levels(DesignRef[[focal_factor_name]])[referencelevel] else {#"B1"
     if (is.character(referencelevel)) {
       if (referencelevel %in% levels(DesignRef[[focal_factor_name]])) Bref <- referencelevel else {stop("referenceLevel not in factor in PRC_scores")}
     }
   }
   DesignRef[[focal_factor_name]]<- Bref
   # predict may give a warning that a term is not a factor while it is... For this reason suppressWarnings()
   reference_scores <- suppressWarnings(stats::predict(object, newdata= DesignRef, rank =ncol(sc$constraints), type = "lc", model = "CCA", scaling = 1, const = myconst))
   #reference_scores <- stats::predict(object, newdata= DesignRef, rank =ncol(sc$constraints), type = "lc", model = "CCA", scaling = 1, const = myconst)

   coln <- colnames(sc$sites)
   # scale and flip axes


# note that vegan::scores(object, display="sp", choices = 1:rank , scaling = 0) does not work for capscale
# (number of rows of  object$CCA$v and  object$CA$v differ...s)
# therefore function PRC_scores addresses the internal structure of object$CCA$v for capscale
  if (object$method=="capscale") species_scores <- object$CCA$v[, 1:rank, drop = FALSE] else
     species_scores <- vegan::scores(object, display="sp", choices = 1:rank , scaling = 0)
   if (is.matrix(species_scores)){
     if (scaling == "ms") mult <- sqrt(nrow(species_scores)) else mult <- 1
     species_scores <- species_scores %*% (flip*mult)
     colnames(species_scores) <-  coln
   } else mult <- 1

   flip <- flip / mult
   sc$sites <- sc$sites %*% flip
   sc$constraints <-  sc$constraints %*% flip
   reference_scores <- reference_scores %*% flip

   sc_ref <- sc
   sc_ref$constraints <- sc$constraints - reference_scores
   sc_ref$sites <- sc$sites - reference_scores
   colnames(sc_ref$constraints) <- paste("PRC", 1:rank,sep = "")
   colnames(sc_ref$sites) <- paste("PRC", 1:rank,"_E",sep = "")
   colnames(sc$constraints) <-  coln
   colnames(sc$sites) <-   paste(coln,"_E",sep = "")
   colnames(reference_scores) <- paste(coln,"_Ref",sep = "")
   out <- list(PRCplus = data.frame(data, sc_ref$constraints, sc_ref$sites, sc$constraints, sc$sites, reference_scores),
               species = species_scores,
               reference_scores = reference_scores,
               focal_factor_name= focal_factor_name, referencelevel=Bref)
   return(out)
 }

