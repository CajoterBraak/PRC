data(pyrifos, package = "vegan") #transformed species data from package vegan
#the chlorpyrifos experiment from van den Brink & ter Braak 1999
Design <- data.frame(week=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     dose=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)),
                     ditch = gl(12, 1, length=132))

mod_rda <- vegan::rda( pyrifos ~ dose:week + Condition(week),  data = Design)
Design_w_PRCs <- PRC_scores(mod_rda, focal_factor_name= "dose", referencelevel = "0",
                            rank = 2, data= Design, scaling ="ms", flip = FALSE)

plot_sample_scores_cdt(Design_w_PRCs, plot = "ditch")
plot_sample_scores_cdt(Design_w_PRCs)
plot_sample_scores_cdt(Design_w_PRCs, plot = "Ditch")


