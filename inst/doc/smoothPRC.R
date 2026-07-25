## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PRC)

## ----pyrifosdata--------------------------------------------------------------
#the chlorpyrifos experiment from van den Brink & ter Braak 1999
data(pyrifos, package = "vegan") #log-transformed species data from package vegan
Y <- pyrifos
Design <- data.frame(Time=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     Treatment=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)),
                     cosm = gl(12, 1, length=132))

## ----setup2-------------------------------------------------------------------
library(LMMsolver)
smoothPRC_model  <- set_smoothPRC_model(data = Design)

## ----models-------------------------------------------------------------------
smoothPRC_model$splineH0
smoothPRC_model$spline

## ----run----------------------------------------------------------------------
smooth_PRC <- smoothPRC(Y, lmm_model = smoothPRC_model)
summary(smooth_PRC$obj[[1]])
summary(smooth_PRC$objH0[[1]])

## ----classicalPRC-------------------------------------------------------------
classical_PRC_model <- set_smoothPRC_model(
  fixed= Yb ~ Treatment:Time,
  fixedH0 = Yb ~Time,
  spline = NULL,
  splineH0 = NULL,
  data = Design)
classical_PRC <- smoothPRC(Y, lmm_model = classical_PRC_model, n_axes = 1)
summary(classical_PRC$obj[[1]])
summary(classical_PRC$objH0[[1]])

## ----mod_PRC, fig.width=10, fig.height=5, out.width='100%', fig.align='center'----
mod_prc <- doPRC(pyrifos ~ Treatment:Time + Condition(Time),  data = Design)

## ----plot_smooth, fig.width=10, fig.height=5, out.width='100%', fig.align='center'----
plotsmoothPRC(smooth_PRC, flip = FALSE, threshold = 1)

## ----mod_PRC2, fig.width=10, fig.height=5, out.width='100%', fig.align='center'----
plotsmoothPRC(classical_PRC, flip = TRUE, title ="Classical PRC via smoothPRC")
classical_PRC$eig/mod_prc$CCA$eig[seq_along(classical_PRC$eig)]

