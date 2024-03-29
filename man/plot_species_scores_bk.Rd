% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_species_scores_bk.r
\name{plot_species_scores_bk}
\alias{plot_species_scores_bk}
\title{Vertical ggplot2 line plot of species loadings in a PRC}
\usage{
plot_species_scores_bk(
  species_scores,
  ylab = latex2exp::TeX("$b_k$"),
  threshold = 7,
  y_lab_interval = 0.5,
  speciesname = NULL,
  scoresname = "RDA1",
  selectname = "Fratio1",
  verbose = TRUE
)
}
\arguments{
\item{species_scores}{a species-by-scores matrix, a data frame with rownames (species names) or a tibble
 with variable with name \code{speciesname} containing species names and
 a column or variabe with name \code{scoresname} containing the scores
(default: \code{"RDA1"}), e.g. species scores from library \code{vegan}}

\item{ylab}{y-axis label. Default: $b_k$.}

\item{threshold}{species with criterion (specified by \code{selectname}) higher than the \code{threshold} are displayed.
Default: 7
(which is the threshold F-ratio for testing a single regression coefficient
at \code{p=0.01} with \code{60} df for the error in a multiple regression
of each single species onto the condition and the ordination axis).
If \code{selectname} is not in \code{species_scores}, the \code{threshold} is divided by \code{14}, so that the default is 0.5.}

\item{y_lab_interval}{interval of the y-axis ticks. A tick at no effect (0) is always included; default: 0.5.}

\item{speciesname}{name of the variable containing the species names (default \code{NULL} uses rownames)}

\item{scoresname}{name of the column or variable containing the species scores to be plotted (default \code{"RDA1"})}

\item{selectname}{name of the column or variable containing the criterion for the selection of species to be displayed
Default: \code{"Fratio1"}; if \code{selectname} is not found in \code{species_scores}, set to \code{scoresname}.}

\item{verbose}{logical for printing the number of species with names out of the total number (default: \code{TRUE}).}
}
\value{
a ggplot object
}
\description{
\code{plot_species_scores_bk} creates a vertical line plot of species scores for a PRC diagram with selection criterion
 for which species to plot with names.
}
\details{
The absolute value of the criterion values is taken before selection,
so that also the species scores of the ordination can serve as a criterion (e.g. \code{selectname="RDA1"}).
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
\code{\link{doPRC}}, \code{\link{plotPRC}}
}
