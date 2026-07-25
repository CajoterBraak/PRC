suppressPackageStartupMessages({
  library(LMMsolver)
  library(dplyr)
  library(ggplot2)
  library(PRC)
})

# Fig.1

# Analyze example data ----------------------------------------------------



  data(SimData)
  # extract design
  Design0 <- SimData[,c("A","B")]
  Design0$A <- factor(Design0$A);  Design0$B <- factor(Design0$B)
  #put levels in natural order
  Design0$A<- factor(Design0$A, levels=c(levels(Design0$A)[-2],levels(Design0$A)[2]))
  Y0 <- as.matrix(SimData[,-(1:3)])
  ids <- which(Design0$A %in% "A10" & Design0$B %in% "B5")
  #design <- "Complete" #
  design <- "Empty cell"
  if (design == "Complete"){  Design <- Design0;Y <- Y0} else {Design <-Design0[-ids,]; Y=Y0[-ids,]}
  print(with(Design,table(A,B)))
  names(Design)

# Design and Y for Fig 1 of  ter Braak 2023 ready -------------------------------------------
dim(Y)
str(Design)
names(Design) <- c("Time","Treatment")
mod_prc <- doPRC(Y ~ Treatment:Time + Condition(Time),  data = Design)
#Mod_prc <- vegan::prc(Y, Design$Treatment,Design$Time)
#Mod_prc$CCA$eig# identical to doPRC
b_mod_prc <- vegan::scores(mod_prc,choices= 1, display = "sp")

x_mod_prc <- vegan::scores(mod_prc,choices= 1, display = "lc")

summary(mod_prc)


smoothPRC_model  <- set_smoothPRC_model(data = Design)
smoothPRC_model$spline
smooth_PRC <- smoothPRC(Y, lmm_model = smoothPRC_model)
summary(smooth_PRC$obj)
(cc <- cor(smooth_PRC$B[,1], b_mod_prc))
plot_species_scores_bk(-smooth_PRC$B, scoresname = "B1")
plot_species_scores_bk(b_mod_prc)
plot_smoothPRC_cdt(smooth_PRC, flip= TRUE)
plotsmoothPRC(smooth_PRC, flip= TRUE,threshold = 0)


# Demonstration of the fitted model
x1 <- scale(as.numeric(Design$Time))[,1]
x2 <- scale(as.numeric(Design$Treatment))[,1]

# with the scaling to mean square of 1 of the species scores b
summary(lm(smooth_PRC$X[,1]~ x1+ x2+ x1:x2 )) # R^2 ~1!


plotPRC(mod_prc)

