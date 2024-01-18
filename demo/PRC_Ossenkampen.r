
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

myrda <- vegan::rda( Y~ A:B + Condition(A), data = Design)
print(myrda)
#SubtractReferenceValues has been replaced by the function PRC_scores
#Design_w_PRCs <- PRC_scores(myrda, focal_factor_name= "B", referencelevel = 1, rank = 2, data= Design)
Design_w_PRCs <- PRC_scores(myrda,  rank = 2, data= Design)

print(names(Design_w_PRCs))
print(names(Design_w_PRCs$PRCplus))
print(names(Design_w_PRCs$coefficients))
if (is.list(Design_w_PRCs$coefficients)) print(round(Design_w_PRCs$coefficients[[1]], 2)) else
  print(round(Design_w_PRCs$coefficients, 2))
# note that Design_w_PRCs$coefficients is not used for plotting

plotPRC(Design_w_PRCs, plot = "Plot")
# many species scores are positive (spot the 0 in the right-hand plot), while the PRC scores are mostly negative
# so that the conclusion is that many species decrease after N and NPK fertilization

# left hand plot only:
plot_sample_scores_cdt(Design_w_PRCs, plot = "Plot")
#  alternative species plot: bar plot of first axis scores --------------------------------------------------
sc_spec <- Design_w_PRCs$species
spec.df <- data.frame("Taxon"=rownames(sc_spec),as.data.frame(sc_spec))
spec.selected <- spec.df[Design_w_PRCs$species[,"Fratio1"] >20,]
id <- order(spec.selected$RDA1,decreasing = FALSE)

spec.selectedsort <- spec.selected[id,]
spec.selectedsort$Taxon <- factor(spec.selectedsort$Taxon, levels = spec.selected$Taxon[id])
RDA1.spec <- ggplot2::ggplot(data=spec.selectedsort, ggplot2::aes(x=Taxon, y=RDA1)) +
  ggplot2::geom_bar(stat="identity", fill="steelblue")+
  ggplot2::theme_minimal() + ggplot2::coord_flip()
RDA1.spec
# many species scores are positive, while the PRC scores are mostly negative
# so that the conclusion is that many species decrease after N and NPK fertilization
