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
#' @export
plotsmoothPRC <- function(
    object,
    mod_prc = NULL,
    with_classical_lines = TRUE,
    with_unconstrained_scores = TRUE,
    flip = FALSE,
    widths = c(4, 1),
    title = NULL,
    left = "Treatment curves",
    right = "taxon scores",
    threshold = 0,
    y_lab_interval = 0.5,
    speciesname = NULL,
    selectname = "Fratio",
    verbose = TRUE)
{
  axis <- 1
  if(is.logical(flip)) flip <- ifelse(flip, -1, 1)
  pl.cdt1 <- plot_smoothPRC_cdt(
    object = object,
    mod_prc = mod_prc,
    with_classical_lines = with_classical_lines,
    with_unconstrained_scores = with_unconstrained_scores,
    flip = flip
  )



  if (is.null(title)) {
    title <- "Smooth PRC diagram"
  }

  scoresname <- "RDA"

  selectname <- paste0(selectname, axis) #
  scoresname <- paste0(scoresname, axis)
   if (!(selectname %in% names(object$species))) {
    selectname <- scoresname
   }

  pl.bk1 <- plot_species_scores_bk(
    species_scores = flip*object$b,
    threshold = threshold,
    y_lab_interval = y_lab_interval,
    speciesname = speciesname,
    scoresname = scoresname,
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
