#' @title Define the model for smooth PRC using LMMsolver (Boer 2023) and
#' smoothPRC
#' @inheritParams LMMsolver::LMMsolve
#' @inheritParams PRC::doPRC
#' @param spline A formula for the spline part of the model.
#' Should be of the form "~ spl1D()", ~ spl2D()" or "~spl3D()".
#' Generalized Additive Models (GAMs) can also be used,
#' for example "~ spl1D() + spl2D()".Default: \code{NULL}, resulting
#' in "~spl2D(time, dose, nseg = c(6, 6), degree = 3, pord = 2)" when
#' \code{treatment.coding} is \code{TRUE}
#'  and in, \emph{e.g.}
#' \preformatted{
#' spl1D(time, nseg = 6, degree = 3, pord = 2) +
#' spl1D(D1,   nseg = 6, degree = 3, pord = 2) +
#' spl1D(D2,   nseg = 6, degree = 3, pord = 2) +
#' spl1D(D3,   nseg = 6, degree = 3, pord = 2)
#' }
#' with  \code{treatment.coding} is \code{FALSE},
#' and the factor\code{Treatment} has four levels.
#' When both \code{spline} and \code{splineH0} are \code{NULL}, the
#' resulting formala is \code{NULL}, for both.
#' @param splineH0 As spline but for the null model. Default: a one dimensional
#' smooth in time:
#' \code{~ spl1D(time, nseg = 6, degree = 3, pord = 2)}
#' @param fixed Formula for the fixed part of  the full model.
#' Default \code{y~1}.
#' NB: the response variable needs to be \code{y}, a
#' variable internally generated from the abundance table \code{Y} and
#' the loading (species) vector, that is optimized.
#' See \code{\link[LMMsolver]{LMMsolve}}.
#' @param fixedH0 Formula for the fixed part of the null model.
#' The default is equal to \code{fixed}, \emph{i.e.}
#'  \code{y~1}.
#' @param random  Formula of the form "~ pred" for the random term
#' in the full model fit.
#' The default: \code{NULL} for no random term in the model.
#' @param randomH0 The default is equal to \code{random}.
#' Random term in the null model fit.
#' Can be \code{NULL} for no random term in the null model.
#' @param start_time numeric time of the start of the treatment application.
#' Default: \code{0}.
#' @param treatment.coding character: "quantitative" or "qualitative".
#' Default \code{"quantitative"} for a quantitative
#' analysis using a 2d spline in \code{time} and \code{dose}.
#' Set \code{treatment.coding ="qualitative"} for a qualitative analysis in
#' in which the default spline model is replaced by one
#' containing 1d splines,
#' one 1d-spline for each level of the factor \code{Treatment}.
#' @param pord  integer for the difference order of the
#' spline penalty.
#' Default: 2.
#' No effect if \code{spline} is not \code{NULL}.
#' @param nseg named integer vector of number of segments in P-splines,
#' can be large.
#'  Default \code{ c(time = 6, dose = 6)}.
#'  No effect if \code{spline} is not \code{NULL}.
#' @param degree degree of the B-splines. Default \code{3}.
#' No effect if \code{spline} is not \code{NULL}.
#' @param referencelevel name of the reference treatment. Default: first level of
#' factor(data$Treatment).
#' @param scaling The default gives mean square species scores of 1, otherwise
#' the sum of squares of the species scores is 1. See \link{doPRC}.
#' Default: "ms".
#' @references
#' Boer, Martin P. 2023.
#' Tensor Product P-Splines Using a Sparse Mixed Model Formulation.
#' Statistical Modelling 23 (5-6): 465–79.
#' \doi{10.1177/1471082X231178591}
#'
#' @details
#'  An atypical aspect of the function is that the time and treatment factors
#'  need to have the names Time and Treatment in \code{data}, \emph{i.e.}
#'  \code{set_smoothPRC_model} assumes that the data contain two factor or
#'   character variables
#'  \code{Time} and \code{Treatment} that can be converted to  the numeric
#'   variables \code{time} and \code{dose}, respectively, using
#'  \code{\link[PRC]{fvalues4levels}}. The values of \code{dose} are set to 0 for
#'  negative values of \code{time} and \code{time ==0},
#'  after subtraction of \code{start_time}.
#'  In this case, the default spline model is:
#'  \code{ ~ spl2D(time, dose, seg = c(6, 6), degree = 3, pord = 2)}.
#'  To enable \code{Treatment} to be treated as a factor (instead of as a
#'  quantitative variable), the function generates variables with
#'  names \code{D1}, ..., \code{D}\emph{k}, with \emph{k} the number of
#'  treatments beyond the reference treatment,
#'  if \code{treatment.coding} is \code{FALSE}.
#'  These variables generated in the function, contain the time
#'  since the start of the experiment for each treatment.
#'  \code{D1, D2, ...} should therefore not be variable names in \code{data},
#'  nor should \code{time} or \code{dose} be variable names
#'  in \code{data}; these are reserved names in \code{smoothPRC}.
#'  An example of the formula for \code{spline}
#'  when \code{treatment.coding} is \code{FALSE}
#'  and the number of levels of \code{Treatment} is 4,  is
#' \preformatted{
#' spl1D(time, nseg = 6, degree = 3, pord = 2) +
#' spl1D(D1,   nseg = 6, degree = 3, pord = 2) +
#' spl1D(D2,   nseg = 6, degree = 3, pord = 2) +
#' spl1D(D3,   nseg = 6, degree = 3, pord = 2)
#' }
#' This is the default model if \code{treatment.coding} is \code{FALSE}.
#' The default spline model assumes that the Treatment factor has a quantitative
#' basis, so that the response to Time and Treatment can be analyzed using a
#' 2D spline model.
#' @example demo/smoothPRC_Ossenkampen.r
#' @seealso \code{\link{smoothPRC}}
#' @export
set_smoothPRC_model <-
  function(spline = NULL,
           splineH0 = ~ spl1D(time, nseg = 6, degree = 3, pord = 2),
           fixed = y ~ 1,
           fixedH0= fixed,
           random = NULL, residual = NULL, weights= NULL,
           randomH0 = random,
           treatment.coding = c("quantitative","qualitative"),
           pord = 2,
           nseg =  c(time = 6, dose = 6),
           degree =3,
           start_time = 0,
           referencelevel = NULL,
           method = "rda",
           scaling = "ms",
           data){
    #data with Time and Treatment as factors, modified here to quantitative time and dose
    # pretimes dose to 0
    # start_time = numeric value or vector of length(nrow(data))
    # NB: time = 0 is treated as control (pre-treatment condition).
    treatment.coding <- match.arg(treatment.coding)
    treatment.coding <- if(treatment.coding =="quantitative") TRUE else FALSE
    if (!is.factor(data$Time)) data$Time <- factor(data$Time)

    if (!is.factor(data$Treatment)) data$Treatment <- factor(data$Treatment)
    if (is.null(referencelevel)) referencelevel <- levels(data$Treatment)[1]
    else if(is.numeric(referencelevel)) {
    referencelevel <- levels(data$Treatment)[referencelevel]}
    data$Treatment <- relevel(data$Treatment,ref = referencelevel)
    valsTime<- PRC::fvalues4levels(data, "Time")
    data$time <- valsTime[data$Time] - start_time
    valsTreatment<-  PRC::fvalues4levels(data, "Treatment")
    if (all(valsTreatment == seq_along(valsTreatment))) valsTreatment <- valsTreatment-1

    if (any((100000*valsTreatment - floor(100000*valsTreatment))>0)){
      # more than 5 decimals in dose (valsTreatment); truncate to 3
      valsTreatment<-signif(valsTreatment, digits = 3)
      levels(data$Treatment) <-valsTreatment
    }

    data$dose <- valsTreatment[data$Treatment]
    data$dose  <- ifelse(data$time>0, data$dose, 0)
    if (!treatment.coding) {
      datI <- model.matrix(~ time + Treatment:time, data = data)[,-c(1,2)]

      datI <- as.data.frame(datI)
      idsl <- 1:(nlevels(data$Treatment)-1)
      names(datI) <- paste0("D", idsl)
      if(!is.null(splineH0) && is.null(spline)){
        spline <- paste0("~spl1D(time, nseg =",
                      nseg["time"],", degree =", degree[1],", pord = ", pord,")")
        txt<-  paste("spl1D(D",idsl,", nseg=",
                      nseg["time"],", degree =", degree[1],", pord =", pord,")",
                      collapse = "+",sep ="")
        spline <- as.formula(paste0(spline,"+",txt))
      }
      datI[data$time <= 0, ] <- 0
      data <- as.data.frame(cbind(data,datI))
    } else {
      if(!is.null(splineH0) && is.null(spline))
        spline <- as.formula(paste0("~ spl2D(time, dose, nseg = c(",
                  nseg["time"],",",nseg["dose"],"), degree =", degree[1],
                  ", pord = ", pord,")"))

    }
    if(!(nseg["time"] == 6 && degree ==3 && pord==2))
    splineH0 <- as.formula(
      paste0("~spl1D(time, nseg =",
             nseg[1],", degree =", degree,", pord = ", pord,")")
    )
#    dat0 <- generate_dat2dat0(data,reflevel = levels(data$Treatment)[1])
    ini_fixed   <-  ~ Treatment:Time
    ini_fixedH0 <-  ~ Time
    out <- list(spline,splineH0,fixed, fixedH0, random,randomH0, residual,
                weights, scaling, data,
                treatment.coding, method, ini_fixed,ini_fixedH0)
    names(out) = c("spline","splineH0","fixed","fixedH0", "random","randomH0", "residual",
                   "weights","scaling","data",
                   "treatment.coding", "method","ini_fixed", "ini_fixedH0")
    class(out) <- c("smoothPRC_LMMsolver", "smoothPRC", "list")
    return(out)
  }
#' @noRd
#' @keywords internal
generate_dat2dat0 <-function(data,reflevel = levels(data$Treatment)[1]){
  #to obtain dat0 from data: set for all samples
  #                         dose to 0 and
  #                         Treatment to the reference level reflevel
  dat0 <- data
  dat0$dose <- 0
  dat0$Treatment <- reflevel
  idsl <- 1:(nlevels(data$Treatment)-1)
  if("D1" %in% names(dat0)) {
    id <- which(names(dat0) %in% "D1")
    dat0[, idsl + (id-1)] <- 0
  }
  return(dat0)
}
#' @noRd
#' @keywords internal
#' @exportS3Method
fit_full_model_rows.smoothPRC_LMMsolver <-
  function(model, ..., y, weights, axes, options_fit)
  {
    st <- options_fit$state
    # first iteration: no previous state available
    if (is.null(st)) {
      st <- list(
        theta = NULL,
        maxit = 10,
        tolerance = 1e-3
      )
    }
    LMMsolver::LMMsolve(
      fixed    = model$fixed,
      spline   = model$spline,
      random   = model$random,
      residual = model$residual,
      weights  = "wrows",
      data     = cbind(
        model$data,
        y     = y,
        wrows = weights$R
      ),
      theta     = st$theta,
      maxit     = st$maxit,
      tolerance = st$tolerance
    )
  }

#' @noRd
#' @keywords internal
#' @exportS3Method
fit_null_model_rows.smoothPRC_LMMsolver <-
  function(model, ...,  y, weights, axes, options_fit)
  {
    # if (is.null(model$fixedH0 )&& is.null(model$splineH0)
    #     && is.null(model$residual ) ) return(NULL)
    if (is.logical(all.equal(model$fixedH0, y~1)) &&
        is.null(model$splineH0) &&
       is.null(model$residual))return(NULL)
    st <- options_fit$state
    # first iteration: no previous state available
    if (is.null(st)) {
      st <- list(
        theta = NULL,
        maxit = 10,
        tolerance = 1e-3
      )
    }
    LMMsolver::LMMsolve(
      fixed    = model$fixedH0,
      spline   = model$splineH0,
      random   = model$randomH0,
      residual = model$residual,
      weights  = "wrows",
      data     = cbind(
        model$data,
        y     = y,
        wrows = weights$R
      ),
      theta     = st$theta,
      maxit     = st$maxit,
      tolerance = st$tolerance
    )
  }
#'
#' @noRd
#' @keywords internal
#' @exportS3Method
update_fit_state.LMMsolve <- function(object, state = NULL)
{
  if (is.null(state) || is.null(state$theta_old)) {
    maxit <- 10
    tol <- 1e-3
  } else {
    dtheta <- sqrt(sum((object$theta - state$theta_old)^2))/
    (sqrt(sum(state$theta^2)) + 1e-12)
    tol <- min(1e-2, max(1e-6, dtheta / 100))
    maxit <- min(50, max(5, ceiling(-log10(tol)) * 5))
  }

  list(
    theta = object$theta,
    theta_old = state$theta,
    maxit = maxit,
    tolerance = tol
  )
}
#'
#' @noRd
#' @keywords internal
#' @exportS3Method
initialize_pwr_alg.smoothPRC_LMMsolver <- function(model, Y, weights, axes)
{

  X <- model.matrix(model$ini_fixed, data = model$data)
  Z <-  model.matrix(model$ini_fixedH0, data = model$data)[,-1]
  Z <- cbind(Z, axes)
  n <- nrow(Z)

  if (model$method == "dbrda" || inherits(Y, "dist") ||
         ((diff(dim(Y)) == 0) && sum(diag(as.matrix(Y))) == 0)) {
    X <- qr.resid(qr(Z), X)
    qrX <- qr(X)
    #print(class(Y))
    Yfit <- qr.fitted(qrX, as.matrix(Y))
    Yfit <- qr.fitted(qrX, t(Yfit))
    Eig <- eigen(Yfit, symmetric = TRUE)
    init <- Eig$vectors[,1]
   # print("init x for dbrda")
  } else {
  suppressMessages(model.fixed <- if (model$method=="rda"){
    # note:equal weights only
   vegan::rda(X = Y, Y=X, Z = Z)
  } else {
    vegan::cca(X = Y, Y=X, Z =Z)
  }
  )
  init <- vegan::scores(model.fixed,choices= 1, display = "sp")
  # print("b smoothPC but set to default ")
  # rnorm(ncol(Y))
  }
  return(init)

}
