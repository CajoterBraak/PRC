Design <- data.frame(week=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     dose=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)))
fvalues4levels(Design, "week")

# fvalues4levels strips the starting k characters so as to find numbers,e.g.
Design$week2 <- factor(paste("Week", Design$week))
levels(Design$week2)
fvalues4levels(Design, "week2")
# they may not be in increasing order in general,
# but will be after sorting:
sort(fvalues4levels(Design, "week2"))
# the resulting numeric variable:
numvar <- fvalues4levels(Design, "week2")[Design[,"week2"]]
summary(numvar)
length(numvar)== nrow(Design)
