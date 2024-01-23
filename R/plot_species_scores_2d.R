#' @title Species ordination plot
#'
#' @description
#' \code{plot_species_scores_2d} creates an ordination diagram of species with selection criterion
#'  of which species to plot with names.
#'
#' @param  species_scores  a species-by-scores matrix, a data frame with rownames (species names) or a tibble
#'  with variable with name \code{speciesname} containing species names and
#'  a column or variabe with names \code{scorenames} containing the scores
#' (default: \code{c("RDA1","RDA2")} being the species score names from \code{\link[vegan]{rda}}.
#' @param threshold species with criterion (specified by \code{selectname}) higher than the \code{threshold} are displayed.
#' Default: 5
#' (which is the threshold F-ratio for testing two regression coefficients (one for each axis)
#' at \code{p=0.01} with \code{60} df for the error in a multiple regression
#' of each single species onto the condition and the ordination axes).
#' If \code{selectname}  is not in the data the \code{threshold} is divided by ten, so that the default is 0.5.
#' @param speciesname name of the variable containing the species names (default \code{NULL} uses rownames).
#' @param scorenames names of the two columns or variables containing the species scores to be plotted
#' (default \code{c("RDA1","RDA2")}).
#' @param selectname name of the column or variable containing the criterion for the selection of species to be displayed
#' Default: \code{"Fratio2"}; if \code{selectname} is not found in \code{species_scores},
#' set to the Euclidian norm (length) of the vectors defined by row-wise combining the values defined by \code{scorenames}.
#' @param withnames_only logical to plot the species that exceed the \code{treshold} only, i.e. to \emph{not} plot points indicating species
#' that do not reach the \code{threshold}. (Default: \code{FALSE}).
#' @param max.overlaps  exclude text labels that overlap too many things. Default: 10 (see \code{\link[ggrepel]{geom_text_repel}}).
#' @param mult_expand fraction of range expansion of the diagram (default = 0.1). See \code{\link[ggplot2]{expansion}}.
#' @param verbose logical for printing the number of species
#' that \code{\link[ggrepel]{geom_text_repel}} or \code{\link[ggrepel]{geom_label_repel}} will try to name in the plot
#' out of the total number of species (default: \code{TRUE}).
#' @param label.repel logical (default: \code{FALSE}) for using labels in white boxes with borders using \code{\link[ggrepel]{geom_label_repel}}
#' instead of using plain text labels using \code{\link[ggrepel]{geom_text_repel}}.
#' @details
#' The names of species are added using \code{\link[ggrepel]{geom_text_repel}} or \code{\link[ggrepel]{geom_label_repel}}.
#' These functions may not be able to plot the names of all selected species.
#' This issue can be diminished by increasing the value of \code{max.overlaps}.
#'
#' @return  a ggplot object
#' @example demo/PRC_pyrifos_bk.r
#' @seealso \code{\link{doPRC}}, \code{\link{plotPRC2d}}
#' @export

plot_species_scores_2d <- function(species_scores,  threshold=7,
                                   speciesname= NULL, scorenames = c("RDA1", "RDA2"),selectname = "Fratio2", label.repel =FALSE,
                                   withnames_only = FALSE, max.overlaps = 10, mult_expand = 0.1, verbose = TRUE){

  # species_scores is a matrix or dataframe  with rownames and a column with name scoresname (default: "RDA1") (species scores from vegan, for example)
  #

  if (!is.null(speciesname) && speciesname %in% names(species_scores)) {
    sppnames = species_scores[,speciesname]
  } else if ( (is.matrix(species_scores)||is.data.frame(species_scores))&& !is.null(rownames(species_scores)) ) {
    sppnames = rownames(species_scores)
  } else {
    sppnames = 1:nrow(species_scores)
  }

  if (is.matrix(species_scores)) namcols <- colnames(species_scores) else namcols <- names(species_scores)

  if (all(scorenames %in% namcols)) scores <- species_scores[,scorenames] else stop("no score columns found in plot_species_scores")
  if (selectname %in% namcols) selectcrit <- species_scores[,selectname] else {
    selectcrit <- sqrt(scores[,1]^2+ scores[,2]^2); threshold =threshold/ 10
    }


  species=data.frame(species=sppnames,scores= scores,selectcrit = selectcrit)
  names(species) <- c("species",scorenames, "selectcrit")
  #
  species.sub=subset(species,abs(selectcrit)>threshold)

if (withnames_only) species <- species.sub
pl_species<-   ggplot2::ggplot(data= species,ggplot2::aes(x= .data[[scorenames[1]]],y = .data[[scorenames[2]]])) +
  ggplot2::geom_point(colour= "grey") +
  ggplot2::coord_fixed(ratio=1)+
  ggplot2::geom_abline(slope =  1,intercept = 0)+
  ggplot2::geom_abline(slope = -1,intercept = 0)+
  ggplot2::geom_hline(yintercept=0) + ggplot2::geom_vline(xintercept =0)



  if (nrow(species.sub)==0) {
    warning("After thresholding, no named species left to display in plot_species_scores_2d (perhaps from plotPRC2d).")
  } else {

    if (verbose) cat( nrow(species.sub)," species with names out of", nrow(species) , "species\n")
    if (label.repel)  geom_text_repel2 <- ggrepel::geom_label_repel else geom_text_repel2 <- ggrepel::geom_text_repel

   pl_species <- pl_species +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult= mult_expand))+ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult= mult_expand))+
    ggplot2::geom_point(data= species.sub,colour= "red")+
    geom_text_repel2(data= species.sub,ggplot2::aes(label = .data[["species"]]),
                                size=7*(5/14),fontface = "italic",max.overlaps = max.overlaps)
  # }



  }

return(pl_species)
}
