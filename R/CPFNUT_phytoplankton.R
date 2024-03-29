#' @title Phytoplankon data from the chlorpyrifos-nutrient (CPF-NUT) experiment from van den Brink & ter Braak (1998)
#'

#' @description The dataframe \code{CPTNUT_phytoplankton} contains the design and phytoplankton data from
#' the microcosm example of PRC from van den Brink & ter Braak (1998). In this experiment from van Donk et al. (1995),
#' there are three
#' treatments: control (\code{con}), nutrients (\code{nut}) and nutrients with chlorpyrifos application (\code{cpf-nut}), each with
#' four cosms (replications) sampled 13 times (twice before nutrients where added and six times before the additional
#' application of chorpyrifos). The phytoplankton data are counts and there are 26 species in total.
#'
#'

#'
#'
#'
#' \itemize{
#' \item\code{samples} character identifier for the sample.
#' \item\code{Time} factor coding for the week of sampling with respect to first application of nutrients.
#' \item\code{Treatment}  treatment factor as in van den Brink & ter Braak (1998).
#' The levels are \code{con}, \code{nut} and \code{cpf-nut} for the four control cosms,
#' the four cosms that receive nutrients after the first
#' two sampling points, and the four cosms to which both nutrients and chorpyrifos are applied, respectively.
#' Note that the cosms of the nutrient-chlorpyrifos treatment are coded herein as \code{nut} in the weeks
#' before the first chlorpyrifos application, as they are treated experiment-wise similarly in these weeks. But, note that
#' the samples from \code{con} and \code{nut} are coded differently on pre-treatment times (although they
#' are similar experiment-wise),
#' so as to see the pre-treatment variability. See also the factor \code{con_nut_cpf}.
#' \item\code{Plot} numeric identifier for the microcosm.
#' \item\code{con_nut_cpf} treatment factor. Before application of nutrients or
#' chlorpyrifos, the samples are all treated similarly, but are coded differently (as often in PRC).
#' \item cell counts of 25 different phytoplankton taxa in 1L and density for the 26th species, Volvox.
#' For details, see van den Donk et al. (1995).
#' }
#' @name CPTNUT_phytoplankton
#' @docType data
#' @references
#'van den Brink, PJ & ter Braak, CJF (1998). Multivariate analysis of stress
#'on experimental ecosystems by principal response curves and similarity analysis.
#' Aquatic ecology, 32, 163-178. https://doi.org/10.1023/A:1009944004756
#'
#' Van Donk E, Prins H, Voogd HM, Crum SJH and Brock TCM
#' (1995) Effects of nutrient loading and insecticide application
#' on the ecology of Elodea – dominated freshwater microcosms.
#' I. Responses of plankton and zooplanktivorous insects. Arch
#' Hydrobiol 133: 417–439. https://doi.org/10.1127/archiv-hydrobiol/133/1995/417
NULL
