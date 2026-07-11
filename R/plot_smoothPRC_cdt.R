#' @title PRC diagram of treatment and plot lines or points without species loading
#'
#' @description
#' \code{plot_smoothPRC_cdt} creates a PRC diagram of the treatments without species loadings.
#'
#' @param  object  a result of \code{\link{smoothPRC}}.
#' @example demo/smoothPRC_pyrifos.r
#' @importFrom stats cor relevel
#' @export
plot_smoothPRC_cdt <- function(object,
                               mod_prc = NULL,
                               with_classical_lines= TRUE,
                               with_unconstrained_scores = TRUE,
                               flip = FALSE){

  # sampled time points only-----
  flip <- ifelse(flip, -1,1)
  if (is.null(mod_prc)) suppressMessages(
    mod_prc <- doPRC(object$Y ~ Treatment:Time + Condition(Time),
                     data = object$lmm_model$data))
  newdat<- cbind(object$lmm_model$data, PRCsmooth = object$PRC, PRCstar = object$PRCstar,
                 PRCclassical = mod_prc$PRCplus$PRC1)
  cc <- cor(newdat$PRCclassical, newdat$PRCsmooth)
  #names(newdat)
  smooth_prc_df <- newdat |>
    mutate(
      Time = time,
      PRCsmooth  = flip* PRCsmooth,
      PRCstar =  flip*PRCstar,
      PRCclassical = sign(cc)*flip*PRCclassical,
      Treatment = Design$Treatment
    ) |>
    select(Time, PRCsmooth, PRCstar, PRCclassical, Treatment)

#dense data----

  #dense -----
  time_points <- sort(unique(object$lmm_model$data$time))
  time_first_applied <- min(time_points[time_points>0])

  npos <- length(time_points[time_points>0])
  nneg <- length(time_points) - npos

  tgrid_dense <- c(seq(min(time_points),0,length= 10*nneg),
                   seq(time_first_applied, max(time_points), length = 10*npos))
  dose_levels <- sort(unique(object$lmm_model$data$dose))
  newdat <- expand.grid(
    time = c(time_points,tgrid_dense),
    dose = dose_levels
  )
  newdat1 <- newdat
  newdat$dose[newdat$time <= 0] <- 0
  pred1 <- predict(object$obj, newdata = newdat)

  newdat0 <- newdat
  newdat0$dose <-  0
  pred0 <- predict(object$obj, newdata = newdat0)
  newdat1$ypred <- (pred1$ypred - pred0$ypred) / object$mult

  smooth_prc_df2 <- newdat1 |>
    mutate(
      Time = time,
      PRCsmooth  = flip * ypred,
      Treatment = factor(dose)#, levels = levels(object$lmm_model$data$Treatment))
    ) |>
    select(Time, PRCsmooth, Treatment)
  levels(smooth_prc_df2$Treatment) <- levels(object$lmm_model$data$Treatment)


  p1 <- ggplot(smooth_prc_df,
               aes(Time, colour = Treatment,shape = Treatment))+
       geom_line(data = smooth_prc_df2,aes(y = PRCsmooth), linewidth = 1.2)
  #  with PRCstar points and classical PRC----


  if (with_classical_lines){
   p1 <-
     p1 +
     geom_line(aes(y = PRCclassical), linewidth = 0.9,linetype = "dotted") +
     geom_point(aes(y = PRCclassical))
  } else{
      p1<- p1 +geom_point(aes(y = PRCsmooth))
    }

  p1
  if (with_unconstrained_scores)
    p1 <- p1 +
    geom_point(aes(y = PRCstar), size = 1.8, color = "grey50") +
    scale_colour_brewer(palette = "Dark2")

  p1 <- p1 +
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
return(p1)
}
