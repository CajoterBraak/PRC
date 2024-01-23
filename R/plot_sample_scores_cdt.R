#' @title PRC diagram of treatment and plot lines or points without species loading
#'
#' @description
#' \code{plot_sample_scores_cdt} creates a PRC diagram of the treatments without species loadings.
#'
#' @param  object  a result of \code{\link{doPRC}} or \code{\link{PRC_scores}} or a list with element
#' PRCplus (a data frame with a variables to plot, see details).
#' @param treatment character vector of names of factors in \code{object$PRCplus},
#'  the first two of which set the \code{color} and \code{linetype} of treatments.
#' Default: derived from \code{object$'focal_and_conditioning_factors'}.
#' @param condition character vector of names of factors in \code{object$PRCplus}, the first one being used as x-axis.
#'  Default: derived from \code{object$'focal_and_conditioning_factors'}.
#' @param plot  0 or 1 (no/yes individual plot points) or
#' character name of a factor in \code{object$PRCplus} indicating the plots
#' (microcosms, subjects or objects) in a longitudinal study so as to plot lines.
#' Default: 1 yielding points instead of lines.
#' @param xvals numeric values for the levels of the condition factor,
#' or name of numeric variable in \code{object$PRCplus} to be used for the x-axis. Default: NULL for which the values are
#' obtained using the function \code{\link{fvalues4levels}} applied to \code{object$PRCplus[[condition[1]]]}
#' @param score_name name of scores to use as y-axis; default: "PRC1".
#' @param symbols_on_curves logical, draw symbols on the PRC curves at the measurement times (default FALSE).
#' If \code{TRUE}, symbols are for the treatment, if there is one, and for the second treatment if there are two.
#' @param size size of symbols and symbols in the legend. default: c(2, 2). If the length is not 2 than the first entry is replicated.
#' @param verbose logical for printing the current model, \code{score_name}, \code{condition} and \code{treatment} (default TRUE).
#' @details
#' \code{plot_sample_scores_cdt} can be used to
#' plot any ordination score or variable against the condition with treatments indicated by colors and line type
#' by setting \code{score_name} to the name of the variable of interest and argument \code{plot} to 0.
#' If object is not a result of \code{\link{doPRC}} or \code{\link{PRC_scores}}), set \code{treatment} and \code{condition}
#' (or \code{object$'focal_and_conditioning_factors'}) and set \code{object} to a list with element \code{PRCplus};
#' \code{object$PRCplus} should contain the names indicated by the arguments treatment, condition and, optionally, plot
#' \itemize{
#' \item{treatment}{: a factor for the treatments},
#' \item{condition}{: a factor or variable condition}
#' }
#' If \code{plot!=0}, the scores (values) for individual sample points or lines must be in the variable "PRC1_E" or,
#' in general, \code{paste(score_name,"_E", sep = "")}.
#' @return  a ggplot object
#' @example demo/PRC_pyrifos_cdt.R
#' @seealso \code{\link{PRC_scores}}, \code{\link{get_focal_and_conditioning_factors}}, \code{\link{plotPRC}} and \code{\link{fvalues4levels}}
#' @export
plot_sample_scores_cdt <- function(object, treatment= NULL, condition = NULL,  plot=1, xvals = NULL, score_name ="PRC1",
                                   size= c(2,2), symbols_on_curves= FALSE,  verbose = TRUE){
  # object is Design_w_PRCs
  # treatment, condition (x-axis) are character vectors, the first is used for plotting
  # condition can be the name of a numeric variable in object$PRCplus
  # plot is the name (character) of a factor object$PRCplus; if so, sample points are connected into lines
  # if plot == 0 no sample points are plotted.
  # xvals : values for the levels of object$Design_w_PRCs$PRCplus[[condition[1]]]

  # find condtion for x-axis and treatment for color (or groups)
  if (length(size)!=2) size <- c(size[1],size[1])
  if (is.null(condition)|| is.null(treatment)){
    if (is.null(treatment)) treatment <- object$focal_and_conditioning_factors$`focal factor`
    if(is.null(condition)) {
      condition <- object$focal_and_conditioning_factors$condition[1]
      if( is.null(condition)) {
         if (!is.na(attr(object$focal_and_conditioning_factors, "interaction")) && attr(object$focal_and_conditioning_factors, "interaction") ) {
           if (nlevels(object$PRCplus[[treatment[2]]])>nlevels(object$PRCplus[[treatment[1]]])){
           condition <-  treatment[2]; treatment <- treatment[1]} else {   condition <-  treatment[1]; treatment <- treatment[2]}
         }
       else condition <- treatment[1]
      }

    }
    if (verbose) {
      cat("The analysis is based on model\n",paste(as.character(formula(object))[c(2,1,3)],collapse = ""),
          "\nThe current score, condition and treatment(s) are:\n",
          "score    (y-axis):", score_name, "\ncondition (x-axis):", condition[1], "\ntreatments (color ):", treatment, "\nSet these arguments yourself if you wish otherwise.\n" )
    }
  }
  # change condition from factor to numeric variable

  plotdat <- object$PRCplus
  if (is.character(plotdat[[condition[1]]])) plotdat[[condition[1]]] <- factor(plotdat[[condition[1]]])
  if(is.factor(plotdat[[condition[1]]])){
    if (is.null(xvals)) xvals <- fvalues4levels(object$PRCplus, condition[1])
    # if fvalues4levels gives other numerical values than 1:nlev,
    # make condition a numeric variable, otherways leave it as factor
    if (any(xvals != seq_along(levels(plotdat[[condition[1]]])))) plotdat[[condition[1]]] <- xvals[plotdat[[condition[1]]]]
  } else {
    if(!is.numeric(plotdat[[condition[1]]])) stop("condition must be a factor or a numeric variable in object$PRCplus")
  }

  score_name_E <-paste(score_name, "E", sep ="_")

# start ggplot ------------------------------------------------------------
  if (treatment[1] == condition[1]) {
    if (length(treatment)==1)  pl.cdt1 <- ggplot2::ggplot(plotdat,ggplot2::aes(x = .data[[condition[1]]], y= .data[[score_name]]))
    else  pl.cdt1 <- ggplot2::ggplot(plotdat,ggplot2::aes(x = .data[[condition[1]]], y= .data[[score_name]], color = .data[[treatment[2]]]))

  } else if (length(treatment) ==1) {
    pl.cdt1 <- ggplot2::ggplot(plotdat,ggplot2::aes(x = .data[[condition[1]]], y= .data[[score_name]], color = .data[[treatment[1]]]))
    } else { # length(treatment) >=2) but condition is not equal to treatment
    pl.cdt1 <- ggplot2::ggplot(plotdat,
                                  ggplot2::aes(x = .data[[condition[1]]], y= .data[[score_name]], color = .data[[treatment[1]]], linetype = .data[[treatment[2]]]))
  }
  pl.cdt1  <-   pl.cdt1 + ggplot2::geom_line(linewidth = 1.2) + ggplot2::ggtitle(score_name)
  if (is.factor(plotdat[[condition[1]]]))
    pl.cdt1  <-   pl.cdt1 + ggplot2::geom_line(ggplot2::aes(x=as.numeric(.data[[condition[1]]])),linewidth = 1.2) + ggplot2::ggtitle(score_name)



  if ( plot == 1) pl.cdt1 <- pl.cdt1 + ggplot2::geom_point(ggplot2::aes(y = .data[[score_name_E]]))+
    ggplot2::ylab("Treatment lines and sample points") else {pl.cdt1 <- pl.cdt1 +ggplot2::ylab("Treatment lines")}
  if (is.character(plot)) {
    if (plot %in% names(plotdat)){
      pl.cdt1 <- pl.cdt1 + ggplot2::geom_line(ggplot2::aes(y= .data[[score_name_E]], group = .data[[plot]]))+
      ggplot2::ylab("Treatment and plot lines")
    } else warning(paste(plot, " is not a variable in object$PRCplus",sep=""))
  }

  if ( symbols_on_curves) {
    if (length(treatment) ==2 && !treatment[1] == condition[1])
    pl.cdt1  <-   pl.cdt1 + ggplot2::geom_point(ggplot2::aes(shape = .data[[treatment[2]]]), size = size[1])+
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=size[2]))) else
     pl.cdt1  <-   pl.cdt1 + ggplot2::geom_point(ggplot2::aes(shape = .data[[treatment[1]]]), size = size[1])+
                                                  ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=size[2])))
  }

  if (is.factor(plotdat[[condition[1]]])&& max(nchar(levels(plotdat[[condition[1]]])))> 3){
    pl.cdt1 <- pl.cdt1 + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=-20,hjust=0, vjust = 0.5, size =20))
  }

  return(pl.cdt1)

}

