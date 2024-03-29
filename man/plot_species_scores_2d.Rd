% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_species_scores_2d.R
\name{plot_species_scores_2d}
\alias{plot_species_scores_2d}
\title{Species ordination plot}
\usage{
plot_species_scores_2d(
  species_scores,
  threshold = 7,
  speciesname = NULL,
  scorenames = c("RDA1", "RDA2"),
  selectname = "Fratio2",
  label.repel = FALSE,
  withnames_only = FALSE,
  max.overlaps = 10,
  mult_expand = 0.1,
  verbose = TRUE
)
}
\arguments{
\item{species_scores}{a species-by-scores matrix, a data frame with rownames (species names) or a tibble
 with variable with name \code{speciesname} containing species names and
 a column or variabe with names \code{scorenames} containing the scores
(default: \code{c("RDA1","RDA2")} being the species score names from \code{\link[vegan]{rda}}.}

\item{threshold}{species with criterion (specified by \code{selectname}) higher than the \code{threshold} are displayed.
Default: 5
(which is the threshold F-ratio for testing two regression coefficients (one for each axis)
at \code{p=0.01} with \code{60} df for the error in a multiple regression
of each single species onto the condition and the ordination axes).
If \code{selectname}  is not in the data the \code{threshold} is divided by ten, so that the default is 0.5.}

\item{speciesname}{name of the variable containing the species names (default \code{NULL} uses rownames).}

\item{scorenames}{names of the two columns or variables containing the species scores to be plotted
(default \code{c("RDA1","RDA2")}).}

\item{selectname}{name of the column or variable containing the criterion for the selection of species to be displayed
Default: \code{"Fratio2"}; if \code{selectname} is not found in \code{species_scores},
set to the Euclidian norm (length) of the vectors defined by row-wise combining the values defined by \code{scorenames}.}

\item{label.repel}{logical (default: \code{FALSE}) for using labels in white boxes with borders using \code{\link[ggrepel]{geom_label_repel}}
instead of using plain text labels using \code{\link[ggrepel]{geom_text_repel}}.}

\item{withnames_only}{logical to plot the species that exceed the \code{treshold} only, i.e. to \emph{not} plot points indicating species
that do not reach the \code{threshold}. (Default: \code{FALSE}).}

\item{max.overlaps}{exclude text labels that overlap too many things. Default: 10 (see \code{\link[ggrepel]{geom_text_repel}}).}

\item{mult_expand}{fraction of range expansion of the diagram (default = 0.1). See \code{\link[ggplot2]{expansion}}.}

\item{verbose}{logical for printing the number of species
that \code{\link[ggrepel]{geom_text_repel}} or \code{\link[ggrepel]{geom_label_repel}} will try to name in the plot
out of the total number of species (default: \code{TRUE}).}
}
\value{
a ggplot object
}
\description{
\code{plot_species_scores_2d} creates an ordination diagram of species with selection criterion
 of which species to plot with names.
}
\details{
The names of species are added using \code{\link[ggrepel]{geom_text_repel}} or \code{\link[ggrepel]{geom_label_repel}}.
These functions may not be able to plot the names of all selected species.
This issue can be diminished by increasing the value of \code{max.overlaps}.
}
\examples{
data(pyrifos, package = "vegan") #transformed species data from package vegan
#the chlorpyrifos experiment from van den Brink & ter Braak 1999
Design <- data.frame(week=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     dose=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)))

library(vegan)
mod_rda <- rda( pyrifos ~ dose:week + Condition(week),  data = Design)
species_scores= scores(mod_rda,choices =1:2, display="sp",scaling = 1)
library(PRC)
head(rdacca_taxon_stats(mod_rda)) # stats per species
get_focal_and_conditioning_factors(mod_rda) # factors from the model

species_scores= vegan::scores(mod_rda,choices =1:2, display="sp",scaling = 2)
plot_species_scores_bk(species_scores) # 1-d vertical plot of species scores (a cut of RDA1-scores at 0.5)
plot_species_scores_bk(species_scores, threshold = 14) #  a cut of RDA1-scores at 1.0 (as scoresname is unset)

plot_species_scores_2d(species_scores)
plot_species_scores_2d(species_scores, label.repel = TRUE)
plot_species_scores_2d(species_scores, max.overlaps = 30, label.repel = TRUE)

}
\seealso{
\code{\link{doPRC}}, \code{\link{plotPRC2d}}
}
