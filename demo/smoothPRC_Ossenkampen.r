
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
if (test){
  cntr <- how(plots = Plots(strata =Design$Plot,type = "free"),
              within = Within(type = "none"), blocks = Design$Block, nperm = 99)

  anova(mod_prc, permutations = cntr)

}

b_mod_prc <- vegan::scores(mod_prc,choices= 1, display = "sp")

smoothPRC_model  <- set_smoothPRC_model(fixed = Yb ~ Block,  data= Design,
                                        treatment.level.as.quantity =FALSE, start_time = 1958)
smoothPRC_model$fixed
smoothPRC_model$spline

out <- smoothPRC(Y, lmm_model = smoothPRC_model)
summary(out$obj)
cor(out$b, b_mod_prc)
if (test){
  cntr$nperm <- 19

  an <- anova(out, permutations = cntr, verbose = TRUE)
  an$pval
}

# Plot of smooth PRC for observed time points -----------------------------------------------

newdat<- cbind(smoothPRC_model$data, PRC =out$PRC)
names(newdat)
smooth_prc_df <- newdat |>
  mutate(
    Time = time,
    PRC  = -PRC,
    Treatment = Design$Treatment
  ) |>
  select(Time, PRC, Treatment)


ggplot(data = smooth_prc_df,
       aes(Time, PRC, colour = Treatment, shape = Treatment)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") +
  geom_line(linewidth = 1.3 ) +
  geom_point() +

  scale_colour_brewer(palette = "Dark2") +
  labs(
    x = "Time",
    y = expression(C[t]),
    colour = "Treatment",
    title = "PRC with smooth treatment line"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )
# the straight lines here are an error
plot_sample_scores_cdt(mod_prc)


# Plot of smooth PRC with dense time grid -----------------------------------------------

time_points <- sort(unique(out$lmm_model$data$time))
tgrid_dense <- seq(min(time_points), max(time_points), length = 200)
treatment_levels <- levels(out$lmm_model$data$Treatment)
newdat00 <- expand.grid(
  Treatment = treatment_levels,
  time = c(time_points,tgrid_dense),
  Block = levels(out$lmm_model$data$Block)
)

#newdat00$Dose <- factor(newdat00$dose)
nlDose <- length(treatment_levels)
datIall <- model.matrix(~ time + Treatment:time, data = newdat00)

newdat <- cbind(Block =newdat00$Block, as.data.frame(datIall))
names(newdat)[-(1:3)] <- paste0("D", 1:(nlDose -1))

newdat[newdat$time<0,2+ (1:(nlDose -1))] <- 0
pred1 <- predict(out$obj, newdata = newdat)

newdat0 <- newdat
newdat0[, -(1:3)]<-0
pred0 <- predict(out$obj, newdata = newdat0)

newdat0 <- newdat
newdat <- pred1
newdat$ypred <- pred1$ypred - pred0$ypred

smooth_prc_df <- newdat |>
  mutate(
    Time = time,
    PRC  = -ypred,
    Treatment = newdat00$Treatment
  ) |>
  select(Time, PRC, Treatment)


ggplot(data = smooth_prc_df,
       aes(Time, PRC, colour = Treatment,shape = Treatment)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") +
  geom_vline(xintercept = 0, linewidth = 0.6, colour = "black") +

  geom_line(linewidth = 1.3 ) +
  geom_point( data = subset(smooth_prc_df,Time %in% time_points)) +

  scale_colour_brewer(palette = "Dark2") +
  labs(
    x = "Time",
    y = expression(C[t]),
    colour = "Treatment",
    title = "smooth quantitative PRC",
    subtitle = " smooth PRC  from LMMsolver"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

