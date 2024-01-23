
data("biomonitoring")

names(biomonitoring)[1:4]
Design <- biomonitoring[,c("year","location")]
Design$year <- factor(Design$year)
nlevels(Design$year)

# extract response variables
Counts <- as.matrix(biomonitoring[,-(1:3)]); rownames(Counts)<-biomonitoring$sample
# transform as a multiplicative model is more logical than an additive one.
# But need to add pseudocount to avoid log(0) = infinite


# Analysis of Grave data --------------------------------------------------
# Fig.1
ids_Grave <- which(Design$location=="Grave")
YG <- log(2*Counts[ids_Grave,] + 1)
# Remark: this transformation reproduces the results in van den Brink et al 2009
# http://dx.doi.org/10.1007/s10661-008-0314-6
#pca
Fig1_pca <- vegan::rda(YG)
# percentage explained
round(100*Fig1_pca$CA$eig/Fig1_pca$tot.chi,1)[1:10]
#PC1  PC2  PC3  PC4  PC5  PC6  PC7  PC8  PC9 PC10
#31.3 13.0  7.4  5.4  3.9  3.6  3.2  3.0  2.7  2.2
# legend of Fig.1 gives 31% and 13%
plot(Fig1_pca,display = "sites")
# note that the first axis is mirrored compared to Fig.1 in the paper

# Fig. 2 ------------------------------------------------------------------
DesignG <- Design[ids_Grave,]
#myrda <- rda(Y~year, data = DesignG)
#print(myrda)
RDA_ref92 <- doPRC(YG~year, data= DesignG)

plotPRC(RDA_ref92, threshold = 20) # selection criterion is the F-ratio of species ~ ax1 regression

# here are the species that fit to axis 1 with 30% or more of their total variance
colnames(RDA_ref92$species)
plotPRC(RDA_ref92, threshold = 0.3,  selectname = "Cfit", width = c(3,1))
# or as in the paper Fig. 2, species with score greater than 0.75 in absolute value
plotPRC(RDA_ref92, threshold = 0.75,  selectname = "RDA", width = c(3,1))



# Fig. 3 ------------------------------------------------------------------

Y <- log(2*Counts + 1)

mod_prc <- doPRC(Y~year:location+Condition(year), data = Design)
print(mod_prc)
#                 Proportio Rank
# Total           1.0000
# Conditional     0.2172    8 #22% (between-year variation)
# Constrained     0.3471    9 #35% (between location variation across years)
# Unconstrained   0.4357   90 #44% (within-year variation)
# This is reported as follows in the legend of Fig. 3 with () additions:
# "Forty-three percent (sic,44%) of the total variation in species composition
# could be attributed to within-year variation, another 22% to
# between-year variation. The differences between the sites
# explained 35% of all variation, 69% of which is displayed on
# the y-axis of the first PRC (a) and another 18% (sic, 17%) on the y-axis of
# the second PRC" as noted in the PRC plots below (plotPRC).
#
#Fig3
plotPRC(mod_prc, threshold = 0.8, selectname = "RDA", width= c(2,1))
#Fig3B
plotPRC(mod_prc, axis = 2, threshold = 0.8, selectname = "RDA", width= c(2,1))


#Fig4
plotPRC2d(mod_prc, max.overlaps = 20, title = "Invertebrates of Kampen (Rhine) compared to Grave (Meuse)")
# Note that the relative scaling of the axes in the  species plot in the paper
# reflects the relative importance of the axes, while the PRC curves do so as well.
# The plot thus has a Benzecri scaling as opposed to the (preferred) biplot scaling



