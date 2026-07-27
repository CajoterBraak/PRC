#' @title smooth PRC diagram
#'
#' @description
#' Creates a smooth PRC diagram with treatment curves and
#' a vertical species-loading plot.
#'
#' @inheritParams plot_smoothPRC_cdt
#' @inheritParams plot_species_scores_bk
#' @inheritParams plotPRC
#'
#'@details
#' \code{sample_times_only = TRUE} can be useful for comparison with
#' classical PRC. It avoids interpolation and may work when
#' the default fails to produce a plot.
#'
#' @export
plotsmoothPRC <- function(
    object,
    axis = 1,
    mod_prc = NULL,
    with_classical_lines = TRUE,
    with_unconstrained_scores = TRUE,
    flip = FALSE,
    widths = c(4, 1),
    title = NULL,
    left = "Treatment curves",
    right = "taxon scores",
    threshold = 7,
    y_lab_interval = 0.5,
    speciesname = NULL,
    selectname = "Fratio",
    sample_times_only = FALSE,
    verbose = TRUE)
{
  if(is.logical(flip)) flip <- ifelse(flip, -1, 1)
  pl.cdt1 <- plot_smoothPRC_cdt(
    object = object,
    axis = axis,
    mod_prc = mod_prc,
    with_classical_lines = with_classical_lines,
    with_unconstrained_scores = with_unconstrained_scores,
    flip = flip,
    sample_times_only =sample_times_only
  )





  scoresname <- "PRC"

  selectname <- paste0(selectname, axis) #
  scoresname <- paste0(scoresname, axis)
   if (!(selectname %in% names(object$species))) {
    selectname <- scoresname
   }


  #library(grid)
  #library(gridExtra)

  if (is.null(title)) {
    title <- paste0("Smooth ", scoresname, "-diagram ")


    if (length(object$eig) >= axis + 1){
      subtitle <- paste0(
        "ratio to next axis: ",
        round(object$eig[axis] /  object$eig[axis + 1], 1))
        title <- paste0(title,"(", subtitle, ")")
    }
    title <- grid::textGrob(
      title,
      x = 0,           # left edge
      hjust = 0,       # left justification
      gp = grid::gpar(fontsize = 12, fontface = "bold"))
  }






  pl.bk1 <- plot_species_scores_bk(
    species_scores = flip*object$B,
    threshold = threshold,
    y_lab_interval = y_lab_interval,
    speciesname = speciesname,
    scoresname = paste0("B", axis),
    selectname = selectname,
    verbose = verbose
  )

  gg_object <- gridExtra::arrangeGrob(
    pl.cdt1 +
      ggplot2::ylab(NULL) +
      ggplot2::ggtitle(""),
    pl.bk1,
    ncol = 2,
    widths = widths,
    top = title,
    left = left,
    right = right
  )

  if (verbose) {
    gridExtra::grid.arrange(gg_object)
  }

  out <- list(
    plot = gg_object,
    separateplots = list(
      treatments = pl.cdt1,
      species = pl.bk1
    )
  )

  invisible(out)
}
