#' @title 1d PRC diagram
#'
#' @description
#' \code{plotPRC} creates a one-dimensional PRC diagram with
#' vertical line plot of species loadings from a result of \code{\link{PRC_scores}}.
#' @param title overall title.
#' @param axis axis shown; default 1, showing the first PRC axis.
#' @param width relative width of treatment PRC plot and the loading plot (see \code{\link[gridExtra]{grid.arrange}}).
#' @param left left axis text (see \code{\link[gridExtra]{arrangeGrob}}).
#' @param right right axis text (see \code{\link[gridExtra]{arrangeGrob}})
#' @param speciesname name of the variable containing the species names (default \code{NULL} uses rownames in \code{object$species})
#' @param selectname name of the column or variable containing the criterion for the selection of species to be displayed
#' without \code{".1"} or other \code{axis} number.
#' Default: \code{"Fratio"};if \code{selectname} is not found in \code{object$species_scores}, set to \code{scoresname}.
#' e.g. the \code{"RDA1"} scores.
#' @inheritParams plot_sample_scores_cdt
#' @inheritParams plot_species_scores_bk
#' @details
#' The function creates the treatment PRC diagram using \code{\link{plot_sample_scores_cdt}} and the
#' vertical line plot of loadings using \code{\link{plot_species_scores_bk}}. These ggplot2-objects are
#' combined in a single object using \code{\link[gridExtra]{arrangeGrob}}.
#'
#' @return  a list of a gridExtra object (plot using \code{\link[gridExtra]{arrangeGrob}})
#' and a list of two ggplot2 objects (the treatment PRC-diagram and the vertical line plot).
#' @seealso \code{\link{PRC_scores}}, \code{\link{plot_sample_scores_cdt}},
#' \code{\link{plot_species_scores_bk}} and \code{\link[gridExtra]{grid.arrange}}
#' @example demo/PRC_pyrifos.r
#' @export

plotPRC <- function(object, treatment= NULL, condition = NULL,  plot=1, xvals = NULL, axis = 1,
                     size= c(2,2), symbols_on_curves= FALSE,
                     width = c(4,1),  title = NULL, left ="Treatment and plot scores", right = "taxon scores",
                     threshold=7, y_lab_interval=0.5,
                     speciesname= NULL, selectname = "Fratio",
                     verbose = TRUE){
  # object = Design_w_PRCs
  # if (!is.null(plot)) {
  #   print(plot)
  #   if (!is.character(plot)) { plot <- as.character(substitute(plot)); print(plot)} else {print(2); print(plot)}
  # }
  # else {plot <- ".plot";print(2); print(plot)}
  score_name <- paste("PRC", axis, sep = "")
  selectname <- paste(selectname, axis, sep = "")
  if (object$method =="rda") scoresname <- paste("RDA",axis, sep = "") else if (object$method =="cca") scoresname <- paste("CCA",axis, sep = "") else {
    stop("unknown method in plotPRC;method in object should be rda or cca")
  }

  if (is.null(plot))
  pl.cdt1 <- plot_sample_scores_cdt(object, treatment= treatment, condition = condition,  plot=NULL, xvals = xvals, score_name =score_name,
                                    size= size, symbols_on_curves = symbols_on_curves, verbose = verbose)
  else
    pl.cdt1 <- plot_sample_scores_cdt(object, treatment= treatment, condition = condition,  plot=plot, xvals = xvals, score_name =score_name,
                                      size= size, symbols_on_curves = symbols_on_curves, verbose = verbose)


  pl.bk1 <- plot_species_scores_bk(species_scores= object$species, threshold = threshold,
        y_lab_interval= y_lab_interval,  speciesname = speciesname, scoresname = scoresname , selectname= selectname, verbose = verbose)

  axis_k <- fvalues4levels( data.frame(score_name= factor(score_name)), fctr = "score_name" )
  if (is.null(title)) {
    title <- paste(score_name, "-diagram: ", round(object$percExp[axis_k]), "% of total explained", sep="")
    if (length(object$percExp) >= axis_k+1)
      title <- paste(title," (ratio to next axis: ",round(object$percExp[axis_k]/object$percExp[axis_k+1],1),")" ,sep="")
  }

  gg_object <- gridExtra::arrangeGrob(pl.cdt1+ggplot2::ylab(NULL)+ ggplot2::ggtitle(""),pl.bk1, ncol =2,widths = width,
                                       top = title, left =left, right =  right)


  if (verbose) gridExtra::grid.arrange(gg_object)
  out <- list(plot = gg_object, separateplots = list(treatments = pl.cdt1, species = pl.bk1))
  invisible(out)
}
