#' @title Ossenkampen experiment
#'

#' @description The dataframe \code{Ossenkampen} contains the  data from
#' a long-term fertilizer experiment (1958-2007) in grassland with 98 response variables
#' (abundances of the plant species that grew in the Ossenkampen plots, measured as counts in 100 subsamples),
#' as analysed in ter Braak (2023).
#'
#' The experiment started in 1958 with 12 plots arranged in two randomized complete blocks with six treatments:
#' four types of fertilizer (K, P, PK or NPK), liming and no fertilizer.
#' Eight years later (in 1966) each block was extended with two plots, one with N and another with NPK fertilization.
#' The two limed plots behaved rather differently from the rest and are omitted here.
#' In this data, the K and P plots are combined with the no fertilizer plots
#'  so that the control treatment (Cntrl) consisted of 6 plots,
#'  with the aim to make the variability among replicates more interesting.
#'  Three plots of the control and one PK plot were not sampled in 1967 and two NPK samples
#'  were not sampled in 1984; these missing samples were not imputed in this data set.
#'
#' The data analyzed in Berendse et al. (2021) also contained the limed plots and contained, for the multivariate analyses,
#' imputations of the few missing samples after 1966.
#'
#' \itemize{
#'  \item\code{Sample} numeric identifier for sample
#' \item\code{Plot} identifier for sample (detailed fertilizer treatment with Block code)
#' \item\code{Block} numeric identifier of the block (1 or 2)
#' \item\code{A} year of sampling
#' \item\code{B} fertilizer treatment, condensed to four treatments ("Cntrl" is the referencelevel)
#' \item\code{AgrCap} count out of 100 subsamples of species AgrCap, and so on for the remaining 97 species
#' }
#' @name Ossenkampen
#' @docType data
#' @references
#' Berendse, F., Geerts, R.H.E.M., Elberse, W.T., Bezemer, T.M.,
#'  Goedhart, P.W., Xue, W., Noordijk, E., ter Braak, C.J.F. & Korevaar, H. (2021)
#'  A matter of time: Recovery of plant species diversity in wild plant communities
#'  at declining nitrogen deposition. Diversity and Distributions, 27, 1180-1193.
#'  https://doi.org/10.1111/ddi.13266
#'
#'  Geerts, R.H.E.M., Bufe, C., Schils, P.C., Korevaar, H., Berendse, F. & Struik, P.C. (2021)
#'   The Ossekampen Grassland Experiment: Data underlying the publication:
#'   A matter of time: recovery of plant species diversity in wild plant communities
#'   at declining nitrogen deposition. https://doi.org/10.4121/13200398
#'
#' ter Braak, C.J.F. (2023)
#'  Redundancy analysis includes analysis of variance-simultaneous component analysis (ASCA)
#'  and outperforms its extensions. Chemometrics and Intelligent Laboratory Systems, 104898.
#'  https://www.sciencedirect.com/science/article/pii/S016974392300148X


NULL
