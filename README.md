# PRC

<!-- badges: start -->
<!-- badges: end -->

PRC is small package to create principal response curves (PRC) and plot its diagram. It expresses ordinition scores 
from vegan::rda or cca as deviation from a reference, usually to visualize multivariate A-dependent effects of a treatment B.
The method has been described in ter Braak, C J F. (2023).
Redundancy analysis includes analysis of variance-simultaneous component analysis (ASCA) and outperforms its extensions
Chemometrics and Intelligent Laboratory Systems https://doi.org/10.1016/j.chemolab.2023.104898
See also https://doi.org/10.6084/m9.figshare.22099844 for a test of the function PRC_scores.

## Installation

You can install the released version of PRC from github by
invoking the R-code within an R-console:

``` r
install.packages("remotes")
remotes::install_github("CajoterBraak/PRC", dependencies = TRUE)
```

