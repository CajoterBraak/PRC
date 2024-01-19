
data("Ossenkampen")

# Read example data ----------------------------------------------------



Design <- Ossenkampen[,c("A","B","Plot","Block")]
Design$A <- factor(Design$A);  Design$B <- factor(Design$B)
Design$B <- factor(Design$B, levels = c("Cntrl","PK","NPK","N"))
Design$Plot <- Design$Plot
Design$Block <- factor(Design$Block)

print(with(Design,table(A,B)))

# extract response variables
Y <- as.matrix(Ossenkampen[,-(1:5)]); rownames(Y)<-Ossenkampen$Sample
# transform as a multiplicative model is more logical than an additive one.
# But need to add pseudocount to avoid log(0)= infinite
Y <- log(Y+1)

# PRC via vegan -----------------------------------------------------------

myrda <- rda( Y~ A:B + Condition(A), data = Design)
print(myrda)
#SubtractReferenceValues has been replaced by the function PRC_scores
#mod_prc <- PRC_scores(myrda, focal_factor_name= "B", referencelevel = 1, rank = 2, data= Design)
mod_prc <- PRC_scores(myrda, data= Design)

print(names(mod_prc))
print(names(mod_prc$PRCplus))
print(names(mod_prc$coefficients))
if (is.list(mod_prc$coefficients)) print(round(mod_prc$coefficients[[1]], 2)) else
  print(round(mod_prc$coefficients, 2))
# note that mod_prc$coefficients is not used for plotting

plotPRC(mod_prc, plot = "Plot")
# many species scores are positive (spot the 0 in the right-hand plot), while the PRC scores are mostly negative
# so that the conclusion is that many species decrease after N and NPK fertilization

# left hand plot only:
plot_sample_scores_cdt(mod_prc, plot = "Plot")
#  alternative species plot: bar plot of first axis scores --------------------------------------------------
sc_spec <- mod_prc$species
spec.df <- data.frame("Taxon"=rownames(sc_spec),as.data.frame(sc_spec))
spec.selected <- spec.df[mod_prc$species[,"Fratio1"] >20,]
id <- order(spec.selected$RDA1,decreasing = FALSE)

spec.selectedsort <- spec.selected[id,]
spec.selectedsort$Taxon <- factor(spec.selectedsort$Taxon, levels = spec.selected$Taxon[id])
RDA1.spec <- ggplot2::ggplot(data=spec.selectedsort, ggplot2::aes(x=Taxon, y=RDA1)) +
  ggplot2::geom_bar(stat="identity", fill="steelblue")+
  ggplot2::theme_minimal() + ggplot2::coord_flip()
RDA1.spec
# many species scores are positive, while the PRC scores are mostly negative
# so that the conclusion is that many species decrease after N and NPK fertilization
