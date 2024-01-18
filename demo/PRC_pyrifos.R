data(pyrifos, package = "vegan") #transformed species data from package vegan
#the chlorpyrifos experiment from van den Brink & ter Braak 1999
Design <- data.frame(week=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     dose=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)),
                     ditch = gl(12, 1, length=132))

mod_rda <- vegan::rda( pyrifos ~ dose:week + Condition(week),  data = Design)
print(mod_rda)
#                 Variance Rank
# Total           1.0000
# Conditional     0.2192   10 (between-week variation)
# Constrained     0.3346   44 (between-treatment variation across weeks)
# Unconstrained   0.4462   77 (within-week variation)
#
Design_w_PRCs <- PRC_scores(mod_rda, focal_factor_name= "dose", referencelevel = "0",
                            rank = 2, data= Design, scaling ="ms", flip = FALSE)
# without lines, threshold is 7
# this is about P = 0.01 for PRC1 in the regression "species_k ~ week+PRC1"
sum(Design_w_PRCs$species[,"Fratio1"] >7) # 34
# this retains 34 species to be plotted with names
plotPRC(Design_w_PRCs, width = c(4,1), symbols_on_curves = TRUE)
sum(Design_w_PRCs$species[,"Fratio1"] >7) # 34
# with lines and a stronger threshold
sum(Design_w_PRCs$species[,"Fratio1"] >10) # 29
plotPRC(Design_w_PRCs, plot = "ditch", threshold = 10,width = c(4,1))

# modifying the plot
gg <- plotPRC(Design_w_PRCs, plot = "ditch",width = c(4,1), verbose = FALSE)
p1 <- gg$separateplots$treatments + ggplot2::ggtitle(paste("new title:", latex2exp::TeX("$c_{dt}$")))   # PRC plot of samples (c_dt)
p2 <- gg$separateplots$species    + ggplot2::ylab("new title: loadings")# loadings of species  (b_k)
# Assign these plots to symbols and use grid.arrange to produce the plot  you like, for example:
gridExtra::grid.arrange(p1+ ggplot2::ylab("")+ ggplot2::xlab("week since chorpyrifos application") + ggplot2::ggtitle(""),
                        p2, ncol =2, widths = c(4,1),
                        top = "new title", left ="PRC curves of treaments", right =  "PRC species loadings")

# test significance of PRC/RDA axes ---------------------------------------

## Ditches are randomized, we have a time series, and are only
## interested in the first axis
ctrl <- permute::how(plots = permute::Plots(strata = Design$ditch,type = "free"),
            within = permute::Within(type = "none"), nperm = 99)

vegan::anova.cca(mod_rda, by = "axis",permutations=  ctrl, model = "reduced", cutoff = 0.10)


