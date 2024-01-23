
data("CPFNUT_phytoplankton")

# Read example data ----------------------------------------------------


names(CPFNUT_phytoplankton)[1:6]
Design <- CPFNUT_phytoplankton[,c("Time","Treatment","Plot","con_nut_cpf")]
summary(Design)
print(with(Design,table(Time,Treatment)))

# extract response variables
Counts <- as.matrix(CPFNUT_phytoplankton[,-(1:5)]); rownames(Counts)<-CPFNUT_phytoplankton$sample
# transform as a multiplicative model is more logical than an additive one.
# But need to add pseudocount to avoid log(0) = infinite

Y <- log(Counts+1)
# Remark: this transformation reproduces the results in van den Brink & ter Braak 1998
#         apart from scaling (but the product b_k*c_dt is identical) and
#         apart from the PRC1 score at week 4, which is erroneous in the paper.
# PRC via vegan -----------------------------------------------------------

mod_prc <- doPRC( Y~ Time * Treatment + Condition(Time),  rank = 3, data= Design, flip= c(FALSE, TRUE, FALSE))
round(mod_prc$percExp, 0)

plotPRC(mod_prc, plot = "Plot", width = c(2,1), symbols_on_curves = TRUE)

plotPRC(mod_prc, plot = "Plot", width = c(2,1), axis = 2)
# note that the default selection criterion "Fratio2" also selects species strongly reacting to axis 1

plotPRC2d(mod_prc, plot = 0, threshold = 1,title = "PRC Phytoplankton van den Brink & ter Braak (1998)")

# modifying the plot
# Assign the plots to symbols and use grid.arrange to produce the plot you like, for example:
gg <- plotPRC2d(mod_prc, plot = 0, verbose = FALSE)
pl_species <- gg$separateplots$species # species ordination
pl <- gg$separateplots$treatments
names(pl)

layout<- rbind(c(1,2), c(3,4),c(3,5))
gridExtra::grid.arrange(pl[["PRC1"]]+ggplot2::ylab("PRC1")+ggplot2::ggtitle(""),
                        pl[["PRC2"]]+ggplot2::ylab("PRC2")+ggplot2::xlab("")+ggplot2::theme(legend.position="none")+ggplot2::ggtitle(""),
                       pl_species,
                       pl[["line11"]]+ggplot2::ylab("scores x = y")+ggplot2::xlab("")+ggplot2::theme(legend.position="none")+
                              ggplot2::ggtitle(""),
                       pl[["line1-1"]]+ggplot2::ylab("scores x = -y")+ggplot2::theme(legend.position="none")+
                         ggplot2::ggtitle(""),
                       layout_matrix = layout, widths = c(3,2),
                       top = "mytitle", left ="", right =  "")



