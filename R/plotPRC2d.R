#' @title 2d PRC diagram
#'
#' @description
#' \code{plotPRC2d} creates a two-dimensional PRC diagram with species loadings from a result of \code{\link{PRC_scores}}
#' @param xylab character vector of length 2 defining x- and y-axis title of the species ordination so as to overwrite the defauls axis labels
#' @param selectname name of the column or variable containing the criterion for the selection of species to be displayed
#' Default: \code{"Fratio"}; if \code{selectname} is not found in \code{object$species_scores},
#' set to the Euclidian norm (length) of the vectors defined by row-wise combining the values defined by \code{scorenames}.
#' @param axes axes shown; default \code{c(1,2)} showing the first two PRC axes.
#' @inheritParams plot_species_scores_2d
#' @inheritParams plot_sample_scores_cdt
#' @inheritParams plotPRC
#' @details
#' The function creates the treatment PRC diagrams for the \code{axes} using \code{\link{plot_sample_scores_cdt}}, with diagrams for the
#' \code{x=y} and \code{x=-y} directions, and the
#' 2d species loading plot using \code{\link{plot_species_scores_2d}}. These ggplot2-objects are
#' combined in a single object using \code{\link[gridExtra]{arrangeGrob}}.
#'
#' @return  a list of a gridExtra object (plot using \code{\link[gridExtra]{arrangeGrob}})
#' and a list of five ggplot2 objects, arranged in a list of two: treatments = the  PRC1, PRC2-diagrams with line x=y and x= -y diagrams
#' and and species= the 2d species ordination plot).
#' @seealso \code{\link{doPRC}}, \code{\link{plot_sample_scores_cdt}},
#' \code{\link{plot_species_scores_2d}} and \code{\link[gridExtra]{grid.arrange}}
#' @example demo/PRC_CPFNUTphytoplankton.r
#' @export

plotPRC2d <- function(object, treatment= NULL, condition = NULL,  plot=1, xvals = NULL, axes = c(1,2),
                     threshold=7, title = NULL, left =NULL, right = NULL,
                     speciesname= NULL, selectname = "Fratio", xylab = NULL,
                     label.repel =FALSE, withnames_only = FALSE, max.overlaps = 10, mult_expand = 0.1, widths = c(3,2),
                     verbose = TRUE){

  if (object$method =="rda") mymethod <- "RDA" else if (object$method =="cca") mymethod <- "CCA" else {
    stop("unknown method in plotPRC;method in object should be rda or cca")
  }
  sn <- paste("PRC", axes, sep = "")
  pl.cdt1 <- plot_sample_scores_cdt(object, treatment = treatment, condition = condition, plot = plot, xvals = xvals, score_name = sn[1], verbose = FALSE)
  pl.cdt2 <- plot_sample_scores_cdt(object, treatment = treatment, condition = condition, plot = plot, xvals = xvals, score_name = sn[2], verbose = FALSE)
  b_coefs <- matrix(c(1,1,1,-1),nrow = 2, ncol=2)
  rownames(b_coefs) <- c("line11","line1-1")
  PRClines <- PRC12(b_coefs , data = object$PRCplus, axes = axes)
  object$PRCplus <- cbind(object$PRCplus,PRClines)
  pl11 <- plot_sample_scores_cdt(object, treatment = treatment, condition = condition, plot = 0, xvals = xvals, score_name = "PRC12_b.line11", verbose = FALSE)
  pl1_1 <- plot_sample_scores_cdt(object, treatment = treatment, condition = condition, plot = 0, xvals = xvals, score_name = "PRC12_b.line1-1", verbose = FALSE)



 # fPRC <- function(k) paste("PRC", k,  ": ", round(percExp[k],0),"% of total explained",sep= "")
  scorenames <- paste(mymethod, axes, sep = "") # to generalize to cca
  selectname <- paste(selectname, axes[2], sep = "")
  if (is.null(xylab)) xylab <- paste(c("x:", "y:"),"PRC", axes,  " (", round(object$percExp[axes],0),"% of total explained)",sep= "")

  pl_species <- plot_species_scores_2d(object$species,  threshold=threshold, speciesname= speciesname,
    scorenames = scorenames, selectname = selectname, label.repel =label.repel,
    withnames_only = withnames_only, max.overlaps = max.overlaps, mult_expand = mult_expand, verbose = verbose)+
    ggplot2::xlab(xylab[1])+ggplot2::ylab(xylab[2])

 # if (is.null(title) ) title("Two-dimensional PRC diagram")
  layout<- rbind(c(1,2), c(3,4),c(3,5))
  gg_object <- gridExtra::arrangeGrob(pl.cdt1+ggplot2::ylab("PRC1")+ggplot2::ggtitle(""),
               pl.cdt2+ggplot2::ylab("PRC2")+ggplot2::xlab("")+ggplot2::theme(legend.position="none")+ggplot2::ggtitle(""),
               pl_species,
               pl11+ggplot2::ylab(latex2exp::TeX("line $x = y$"))+ggplot2::xlab("")+ggplot2::theme(legend.position="none")+ggplot2::ggtitle(""),
               pl1_1+ggplot2::ylab(latex2exp::TeX("line $x = -y$"))+ggplot2::theme(legend.position="none")+ggplot2::ggtitle(""),
               layout_matrix = layout, widths = widths,
               top = title, left =left, right =  right)

  if (verbose) gridExtra::grid.arrange(gg_object)
  out <- list(plot = gg_object, separateplots = list(treatments = list(PRC1=pl.cdt1,PRC2=pl.cdt2,line11 = pl11, `line1-1`= pl1_1), species = pl_species))
  invisible(out)
}

PRC12 <- function(b_coefs= c(1,1), axes = c(1,2), data ){
  # gives curves for combinations (defined by b_coefs) of the first two PRC axes (PRC1 & PRC2)
  # data (Design_w_PRCs$PRCplus)  object from PRC::PRC_scores
  # b_coefs (RV by n-Axis) matrix or vector of b-coefs for each axis
  # value: matrices PRC12_b and PRC12_b_E
  sn <- paste("PRC", axes, sep = "")
  sn_E <- paste(sn,"_E",sep = "")
  if (!is.matrix(b_coefs)) b_coefs <- matrix(b_coefs, nrow= 1)
  PRC12_b <-PRC12_b_E <-matrix(NA, nrow = nrow(data), ncol = nrow(b_coefs))
  colnames(PRC12_b) <- colnames(PRC12_b_E) <- rownames(b_coefs)
  for (k in 1:nrow(b_coefs)){
    PRC12_b[,k]<- b_coefs[k,1]*data[[sn[1]]] + b_coefs[k,2]*data[[sn[2]]]
    PRC12_b_E[,k]<- b_coefs[k,1]*data[[sn_E[1]]] + b_coefs[k,2]*data[[sn_E[2]]]
  }
  colnames(PRC12_b_E)  <- paste(colnames(PRC12_b_E), "_E",sep ="" )
  return(list(PRC12_b=PRC12_b, PRC12_b_E=PRC12_b_E ))
}
