
## ---- Packages ----

suppressPackageStartupMessages({
  library(LMMsolver)
  library(dplyr)
  library(ggplot2)
  library(PRC)
})



data("Ossenkampen")
names(Ossenkampen)[4:5]
names(Ossenkampen)[4:5] <- c("Time", "Treatment")
test <- FALSE
if (test){
  years <- sort(unique(Ossenkampen$Time))
  years<- years[c(-(1:9))]
  years <- years[-which(years%in% c(1984))]
  years
  ids <- which(Ossenkampen$Time %in% years)
}else ids <- 1:nrow(Ossenkampen)

Y <- Ossenkampen[ids,-(1:5)]
Design <- Ossenkampen[ids,1:5]
Design$Block <- factor(Design$Block)
# in doPRC Time must be a factor, not a quantative variable
Design$Time <- factor(Design$Time)
with(Design,table(Time,Treatment))
# unbalanced!!! if test <- FALSE

mod_prc <- doPRC(Y ~ Time:Treatment + Condition(Time+Block),  data = Design)


b_mod_prc <- vegan::scores(mod_prc,choices= 1, display = "sp")

smoothPRC_model  <-
  set_smoothPRC_model(fixed = Yb ~ Block, data= Design,
                      treatment.level.as.quantity =FALSE, start_time = 1958)
smoothPRC_model$fixed
smoothPRC_model$spline

smooth_PRC <- smoothPRC(Y, lmm_model = smoothPRC_model)
summary(smooth_PRC$obj[[1]])
cor(smooth_PRC$B[,1], b_mod_prc)


plotsmoothPRC(smooth_PRC, flip = TRUE)
