
## ---- Packages ----

suppressPackageStartupMessages({
  library(LMMsolver)
  library(dplyr)
  library(ggplot2)
  library(permute)
  library(PRC)
})






#the chlorpyrifos experiment from van den Brink & ter Braak 1999
data(pyrifos, package = "vegan") #log-transformed species data from package vegan
Y <- pyrifos
Design <- data.frame(Time=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     Treatment=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)),
                     ditch = gl(12, 1, length=132))
mod_prc <- doPRC(pyrifos ~ Treatment:Time + Condition(Time),  data = Design)
cntr <- how(plots = Plots(strata =Design$ditch,type = "free"),
            within = Within(type = "none"), nperm = 99)

anova(mod_prc, permutations = cntr)


b_mod_prc <- vegan::scores(mod_prc,choices= 1, display = "sp")

smoothPRC_model  <- set_smoothPRC_model(data = Design)
smoothPRC_model$spline
out <- smoothPRC(Y, lmm_model = smoothPRC_model)
summary(out$obj)
cor(out$b, b_mod_prc)

cntr$nperm <- 19
an <- anova(out, permutations = cntr, verbose = TRUE)
an$pval


# Plot of smooth PRC for observed time points -----------------------------------------------

newdat<- cbind(out$lmm_model$data, PRC =out$PRC)
names(newdat)
smooth_prc_df <- newdat |>
  mutate(
    Time = time,
    PRC  = PRC,
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
    title = "smooth quantitative PRC"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

plotPRC(mod_prc)


# Plot of smooth PRC with dense time grid -----------------------------------------------

#graph
time_points <- sort(unique(out$lmm_model$data$time))
tgrid_dense <- seq(min(time_points), max(time_points), length = 200)
dose_levels <- sort(unique(out$lmm_model$data$dose))

newdat <- expand.grid(
  time = c(time_points,tgrid_dense),
  dose = dose_levels
)
newdat$dose[ newdat$time <= 0] <- 0
pred1 <- predict(out$obj, newdata = newdat)

newdat0 <- newdat
newdat0$dose <-  0
pred0 <- predict(out$obj, newdata = newdat0)


newdat <- pred1
newdat$ypred <- pred1$ypred - pred0$ypred

smooth_prc_df2 <- newdat |>
  mutate(
    Time = time,
    PRC  = ypred,
    Treatment = factor(dose, levels = levels(Design$Treatment))
  ) |>
  select(Time, PRC, Treatment)
ggplot(data = smooth_prc_df2,
       aes(Time, PRC, colour = Treatment, shape = Treatment)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") +
  geom_line(linewidth = 1.3 ) +
  geom_point( data = subset(smooth_prc_df2,Time %in% time_points)) +

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





