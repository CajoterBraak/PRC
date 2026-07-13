#' @title smooth PRC diagram of treatment and plot lines or points without species loading
#'
#' @description
#' \code{plot_smoothPRC_cdt} creates a PRC diagram of the treatments without species loadings.
#'
#' @param  object  a result of \code{\link{smoothPRC}}.
#' @param mod_prc a results of \code{\link{doPRC}}, \emph{i.e.}
#' a classical PRC with the same data and design.
#' Default \code{NULL}, in which the classical PRC is computed using
#' \code{\link{doPRC}} and the data in \code{object}.
#' @param with_classical_lines logical to add or suppress classical PRC lines.
#' Default \code{TRUE}, \emph{i.e.} with classical PRC lines.
#' @param with_unconstrained_scores logical to add or suppress the unconstrained
#' scores in the smooth PRC showing the scatter of the
#' individual samples (cosms).
#' Default \code{TRUE}, \emph{i.e.} with individual points.
#' @param sample_times_only logical to plotlines based on the sampled times
#' only, \emph{i.e.} without interpolation. Default \code{FALSE}.
#' @param flip logical. Should the axis be reversed?
#' Default \code{FALSE}. Can be numeric with -1 meaning: reverse, and 1 meaning
#' do not reverse.
#' @example demo/smoothPRC_pyrifos.r
#' @importFrom stats cor relevel
#' @importFrom ggplot2 ggplot aes geom_line geom_point
#' @importFrom ggplot2 scale_colour_brewer scale_linetype_manual
#' @importFrom ggplot2 guide_legend guides labs
#' @importFrom ggplot2 theme_bw theme
#' @importFrom ggplot2 element_blank element_text
#' @export
plot_smoothPRC_cdt <- function(object,
                               mod_prc = NULL,
                               with_classical_lines = TRUE,
                               with_unconstrained_scores = TRUE,
                               flip = FALSE,
                               sample_times_only = FALSE
                               )
{
  # sampled time points only-----
  if(is.logical(flip)) flip <- ifelse(flip, -1, 1)

  if (is.null(mod_prc))
    suppressMessages(
      mod_prc <- doPRC(
        object$Y ~ Treatment:Time + Condition(Time),
        data = object$lmm_model$data
      )
    )
  newdat <- cbind(
    object$lmm_model$data,
    PRCsmooth = object$PRC,
    PRCstar = object$PRCstar,
    PRCclassical = mod_prc$PRCplus$PRC1
  )
  cc <- cor(newdat$PRCclassical, newdat$PRCsmooth)
  smooth_prc_df <- data.frame(
    Time = object$lmm_model$data$time,
    PRCsmooth = flip * object$PRC,
    PRCstar = flip * object$PRCstar,
    PRCclassical = sign(cc) * flip * mod_prc$PRCplus$PRC1,
    Treatment = object$lmm_model$data$Treatment
  )

  if (deparse(object$lmm_model$fixedH0) == deparse(Yb ~ Time)||
      sample_times_only) {
    smooth_prc_df2  <- smooth_prc_df
    } else {
    # dense data-----
    time_points <- sort(unique(object$lmm_model$data$time))
    time_first_applied <- min(time_points[time_points > 0])

    npos <- length(time_points[time_points > 0])
    nneg <- length(time_points) - npos

    tgrid_dense <- c(
      seq(min(time_points), 0, length = 10 * nneg),
      seq(time_first_applied,
          max(time_points),
          length = 10 * npos)
    )

    #dose_levels <- sort(unique(object$lmm_model$data$dose))

    newdat <- expand.grid(
      time = c(time_points, tgrid_dense),
      Treatment = factor(levels(object$lmm_model$data$Treatment))
    )

    valsTreatment<- PRC::fvalues4levels(object$lmm_model$data, "Treatment")
    if (all(valsTreatment == seq_along(valsTreatment))) valsTreatment <- valsTreatment-1
    newdat$dose <- valsTreatment[newdat$Treatment]
    newdat$dose  <- ifelse(newdat$time>0, newdat$dose, 0)

    if ("D1" %in% names(object$lmm_model$data)) {
      datI <- model.matrix(~ time + Treatment:time, data = newdat)[,-c(1,2)]

      datI <- as.data.frame(datI)
      idsl <- 1:(nlevels(newdat$Treatment)-1)
      names(datI) <- paste0("D", idsl)
      datI[newdat$time <= 0, ] <- 0
      newdat <- cbind(newdat,datI)
    }

    nams <- names(object$lmm_model$data)
    idsl <- 1:(nlevels(object$lmm_model$data$Treatment)-1)
    nams <- nams[!nams %in% c("Time","Treatment","time","dose", paste0("D", idsl))]

    n <- nrow(newdat)

    avdat <- as.data.frame(lapply(object$lmm_model$data[nams], function(x) {
      if (is.factor(x)) {
        factor(rep(levels(x)[1], n), levels = levels(x))
      } else if (is.character(x)) x[1] else {
        rep(mean(x, na.rm = TRUE), n)
      }
    }))
    newdat <- cbind(newdat, avdat)
    newdat1 <- newdat
    pred1 <- predict(object$obj, newdata = newdat)
    # sets LMMsolver_model with response in formulafixed and data : Design
    newdat0 <- newdat
    newdat0$dose <- 0
    newdat0$Treatment <- levels(object$lmm_model$data$Treatment)[1]
    if("D1" %in% names(newdat0)) {
        id <- which(names(newdat0) %in% "D1")
        newdat0[, id + (idsl-1)] <- 0
      }
    pred0 <- predict(object$obj, newdata = newdat0)

    newdat1$ypred <- (pred1$ypred - pred0$ypred) / object$mult

    smooth_prc_df2 <- data.frame(
      Time = newdat1$time,
      PRCsmooth = flip * newdat1$ypred,
      Treatment = factor(newdat1$dose)
    )

    levels(smooth_prc_df2$Treatment) <-
      levels(object$lmm_model$data$Treatment)
  }

  p1 <- ggplot(
    smooth_prc_df,
    aes(
      x = .data$Time,
      colour = .data$Treatment,
      shape = .data$Treatment
    )
  ) +
    geom_line(
      data = smooth_prc_df2,
      aes(
        y = .data$PRCsmooth,
        linetype = "Smooth"
      ),
      linewidth = 1.2
    )

  if (with_classical_lines) {

    p1 <- p1 +
      geom_line(
        aes(
          y = .data$PRCclassical,
          linetype = "Classical"
        ),
        linewidth = 0.9
      ) +
      geom_point(
        aes(y = .data$PRCclassical)
      )

  } else {

    p1 <- p1 +
      geom_point(
        aes(y = .data$PRCsmooth)
      )

  }

  if (with_unconstrained_scores) {

    p1 <- p1 +
      geom_point(
        aes(y = .data$PRCstar),
        size = 1.8,
        colour = "grey50"
      )

  }
  p1 <- p1 +
    scale_colour_brewer(palette = "Dark2") +
    scale_linetype_manual(
      name = "PRC",
      values = c(
        Smooth = "solid",
        Classical = "dotted"
      ),
      guide = guide_legend(
        override.aes = list(colour = "black")
      )
    ) +
    labs(
      x = "Time",
      y = expression(C[t]),
      colour = "Treatment",
      title = "smooth quantitative PRC"
    ) +
    guides(
      colour = guide_legend(order = 1),
      shape = guide_legend(order = 1),
      linetype = guide_legend(order = 2)
    ) +
  theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(face = "bold"),
      legend.position = "right"
    )

  return(p1)
}
