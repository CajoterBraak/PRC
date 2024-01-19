data(pyrifos, package = "vegan") #transformed species data from package vegan
#the chlorpyrifos experiment from van den Brink & ter Braak 1999
Design <- data.frame(week=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     dose=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)))

# library(vegan)
mod_rda <- rda( pyrifos ~ dose:week + Condition(week),  data = Design)
species_scores= scores(mod_rda,choices =1:2, display="sp",scaling = 1)
# library(PRC)
head(rdacca_taxon_stats(mod_rda)) # stats per species
get_focal_and_conditioning_factors(mod_rda) # factors from the model

species_scores= vegan::scores(mod_rda,choices =1:2, display="sp",scaling = 2)
plot_species_scores_bk(species_scores) # 1-d vertical plot of species scores (a cut of RDA1-scores at 0.5)
plot_species_scores_bk(species_scores, threshold = 14) #  a cut of RDA1-scores at 1.0 (as scoresname is unset)

plot_species_scores_2d(species_scores)
plot_species_scores_2d(species_scores, label.repel = TRUE)
plot_species_scores_2d(species_scores, max.overlaps = 30, label.repel = TRUE)

