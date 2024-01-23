#' @title Calculate principal response curve (PRC) scores from vegan::rda, cca, dbrda or capscale objects
#'
#' @description
#' \code{PRC_scores} calculates scores to display principal response curves with
#' scores for individual plots from a vegan constrained ordination object (\code{\link[vegan]{cca.object}}).
#'
#' @param  object  a \code{vegan} \code{\link[vegan]{cca.object}},
#'  a result of \code{\link[vegan]{rda}}, \code{\link[vegan]{cca}},
#' \code{\link[vegan]{dbrda}} or \code{\link[vegan]{capscale}} with model specified via a formula.

#' @param  focal_factor_name name of one or two focal factors (the treatment(s) of interest); if unspecified,
#' derived from the formula in \code{object} via the function \code{\link{get_focal_and_conditioning_factors}}.
#' @param referencelevel     numeric or character level(s) to be used as reference of the \code{focal_factor_name}(s); default: 1.
#' @param rank               number of axes to be computed; default 2.
#' @param flip               logical value or vector; default: FALSE. Should the axes be flipped,
#' i.e. reversed in orientation? \code{flip} can be numeric with values -1 and 1
#' for reverse and no reverse, respectively,
#' i.e. multiply the scores by either -1 or 1.
#' @param scaling    character \code{c("ms", "ss")}. Scales the axes of an RDA object to mean square species scores of 1 (\code{"ms"}) or
#'   a sum of squares of 1 (\code{"ss"}). The only scaling for a CCA  object is \code{"ss"} (weighted mean square = 1, with weight =
#'   species total abundance). For a dbrda object, there are no species scores and the scaling is like "ss".
#'  The default scaling (\code{"ms"} for RDA and \code{"ss"} for CCA) has the advantage that
#'                    the PRC treatment and sample scores measure the effect sizes of the 'average' species
#'                    as their (weigthed) mean square is 1 (sd = 1); it is the scaling used in Canoco5 (www.canoco5.com).
#'                    Scaling \code{"ss"} is the default in  \code{\link[stats]{prcomp}}
#'                    and used in ter Braak (2023).
#' @param data data frame of the experimental design used in the call
#'  to \code{\link[vegan]{rda}} or \code{\link[vegan]{cca}}
#' @return  A named list:
#' \itemize{
#' \item{PRCplus:} { a data frame that combines the design in \code{data} with the PRC and RDA/CCA scores
#' with ("_E") and without error (the unconstrained [sites] and constrained [LC] scores, respectively),
#' and, finally, the reference scores. Note that
#' PRC1_E = RDA1_E - RDA1_Ref, etc for other axes}
#' \item{species:}  a matrix with the loadings with respect to the PRC scores; NULL if the \code{object} lacks loadings.
#' \item{reference_scores:}  a matrix with the scores of each unit in \code{data} with the level of the focal factor
#'  replaced by the reference level
#' \item{focal_factor_name:}  name of the focal factor in the call to \code{PRC_scores}
#' \item{referencelevel:}  referencelevel in the call to \code{PRC_scores}
#' \item{coefficients:}  list of PRC-coefficients for each axis (matrix if rank is equal to 1)
#' \item{focal_and_conditioning_factors:}{ list of focal and conditioning factors; obtained from
#' \code{\link{get_focal_and_conditioning_factors}} if argument \code{focal_factor_name} in the call is unset.}
#' \item{terms:} object$terms. See \code{\link[vegan]{cca.object}}.
#' \item{terminfo:} object$terminfo. See \code{\link[vegan]{cca.object}}.
#' \item{method:} object$method. See \code{\link[vegan]{cca.object}}.
#' \item{percExp:} percentage variance of each axis out of the variance that can be explained by the predictors and covariates.
#'}
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
#'  The element \code{object$'focal_and_conditioning_factors'} in value is set
#'  by applying the function \code{\link{get_focal_and_conditioning_factors}}
#'  to the ordination model specified in \code{object}.
#'
#'  CCA-based PRC is useful in data with many zeroes and which are strictly compositional,
#'  i.e. have row sums that have no meaning for the question of interest (ter Braak and te Beest, 2022).
#'  A layman's example is that
#'  the weight of a cake is not important for its taste; taste depends on its composition only.
#'  CCA can be applied without log-transforming the data and thereby avoids the issue of
#'  replacement of zeroes (ter Braak and te Beest, 2022).
#'
#' @example demo/PRC_pyrifos_cdt.R
#'
#' @seealso \code{\link[PRC]{doPRC}}, \code{\link[PRC]{plotPRC}}, \code{\link[PRC]{plotPRC2d}}



#' @export

PRC_scores <-  function(object, focal_factor_name = NULL, referencelevel = NULL, rank = 2, flip = rep(FALSE,rank),
                        scaling = "ms",data ){

   if (scaling == "sym") stop("scaling = \"sim\" has not yet been implemented")
   if (object$method == "cca") scaling <- "ss"

   myconst <- sqrt((nobs(object)-1)*object$tot.chi) # scaling of scores and loadings as in prcomp (and this paper).
   #if (scaling == "vegan::prc") myconst = (nobs(object)-1)*object$CCA$eig^0.25
   rank <- min(rank,length(object$CCA$eig))


   sc <- vegan::scores(object, display=c("sites","lc"), choices = 1:rank , scaling = 1, const = myconst)

   if (is.logical(flip)) flip <- ifelse(flip, -1,1)
   if (length(flip)!=rank){
     if (length(flip)>rank) flip = flip[1:rank]  else  flip <- c(flip, rep(flip, rank-length(flip)))
   }
   if (length(flip)==1) flip = matrix(flip,nrow = 1, ncol=1 ) else flip = diag(flip)



   DesignRef <- data

   if (is.null(focal_factor_name)) {
     fc<- get_focal_and_conditioning_factors(object);
     focal_factor_name <- fc$`focal factor`
   } else fc <- NULL

   if (is.null(referencelevel)) referencelevel <- rep(1, length(focal_factor_name))

   #if(!focal_factor_name[1] %in% names(object$terminfo$xlev)) stop("Error in PRC_scores:focal_factor_name[1] not in the model")


   #if (is.null(levels(DesignRef[[focal_factor_name[1]]]))) print(errorCondition("Error in PRC_scores:focal_factor_name[1] or reference erroneous"))
   Bref <- vector(mode = "character", length = length(focal_factor_name))
   for (k in seq_along(focal_factor_name)) {
     if (is.character(DesignRef[[focal_factor_name[k]]]))DesignRef[[focal_factor_name[k]]] <- factor(DesignRef[[focal_factor_name[k]]])
     if (is.null(levels(DesignRef[[focal_factor_name[k]]])))
       print(errorCondition(paste("Error in PRC_scores:focal_factor_name[",k,"] or reference erroneous")))
     if (is.numeric(referencelevel[k]))
     Bref[k] <- levels(DesignRef[[focal_factor_name[k]]])[referencelevel[k]] else {#"B1"
     if (is.character(referencelevel[k])) {
       if (referencelevel[k] %in% levels(DesignRef[[focal_factor_name[k]]])) Bref[k] <- referencelevel[k] else {stop(paste("referencelevel[",k, "] not in factor in PRC_scores"))}
       }
     }
     DesignRef[[focal_factor_name[k]]]<- Bref[k]
   }
   # predict may give a warning that a term is not a factor while it is... For this reason suppressWarnings()
   reference_scores <- suppressWarnings(stats::predict(object, newdata= DesignRef, rank =ncol(sc$constraints), type = "lc", model = "CCA", scaling = 1, const = myconst))
   #reference_scores <- stats::predict(object, newdata= DesignRef, rank =ncol(sc$constraints), type = "lc", model = "CCA", scaling = 1, const = myconst)

   coln <- colnames(sc$sites)
   # scale and flip axes


# note that vegan::scores(object, display="sp", choices = 1:rank , scaling = 0) does not work for capscale
# (number of rows of  object$CCA$v and  object$CA$v differ...s)
# therefore function PRC_scores addresses the internal structure of object$CCA$v for capscale
  if (object$method=="capscale") species_scores <- object$CCA$v[, 1:rank, drop = FALSE] else {
     species_scores <- vegan::scores(object, display="sp", choices = 1:rank , scaling = 0)
  }
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

# obtain coefficients -----------------------------------------------------

   DesignComplete <- expand.grid(object$terminfo$xlev)

   id <- which(duplicated(data[,names(object$terminfo$xlev)]))
   prc_coefs <- cbind(data[-id,names(object$terminfo$xlev),drop = FALSE], sc_ref$constraints[-id,]) # without the missing combinations
   aa <- dplyr::left_join(DesignComplete, prc_coefs, by=names(object$terminfo$xlev) )
   coefs <- list()
   for (k in seq_len(rank)){
     coefs[[k]] <- array(aa[,length(names(object$terminfo$xlev)) + k], dim= sapply(object$terminfo$xlev,length),dimnames = object$terminfo$xlev )
     attr(coefs[[k]], "method") <- paste(object$method, "-based PRC",k, sep ="")
   }
   names(coefs) <- paste("PRC", seq_len(rank), sep = "")

   species_scores <- cbind(species_scores, rdacca_taxon_stats(object, rank = rank))

   if (is.null(fc)) {
     Condi_nams <-get_focal_and_conditioning_factors(object)$condition;
     #Condi_nams <- names(object$terminfo$xlev)[!names(object$terminfo$xlev) %in% focal_factor_name]
     fc <- list(focal_factor_name, Condi_nams)
     names(fc) <-  c("focal factor", "condition")
     if (length(Condi_nams)==1)  attr(fc, "interaction") <- TRUE else attr(fc, "interaction") <- NA
   }
   out <- list(PRCplus = data.frame(data, sc_ref$constraints, sc_ref$sites, sc$constraints, sc$sites, reference_scores),
               species = species_scores,
               reference_scores = reference_scores,
               focal_and_conditioning_factors = fc,
               referencelevel=Bref,
               terms = object$terms,
               terminfo = object$terminfo,
               method= object$method)
   if (rank==1) {
     out$coefficients <- unlist(coefs[[1]])
   } else out$coefficients <- coefs
   out$percExp <-100*object$CCA$eig/sum(object$CCA$eig)
   attr(out$percExp,"meaning") <- "percent of total explained by predictors given covariates"

   class(out) <- c("PRC_scores", class(out))
   return(out)
 }

