fFratios_Time_X <- function(smooth_PRC){
  # initialize
  Z <- model.matrix(~ smooth_PRC$lmm_model$data$Time)
  # residualize X on Time
  qrT <- qr(Z)
  X0 <- qr.resid(qrT, smooth_PRC$X)
  qrx <- qr(X0) # QR of residualized X
  Q   <- qr.Q(qrx)
  # local function
  fseq <- function(y) {
    # sequential SS
    ## needs qrT and Q as above
    # X: matrix of orthogonal columns x1,...,xm
    # residualize y on Time
    y0 <- qr.resid(qrT, y)
    a  <- crossprod(Q, y0)
    ss <- cumsum(drop(a^2))
    rss0 <- sum(y0^2)
    n  <- length(y)
    pT <- qrT$rank
    k  <- seq_along(ss)
    Fratio <- (ss / k) / ((rss0 - ss) / (n - pT - k))
    return(Fratio)
  }
  Fratio <- t(apply(smooth_PRC$Y,2,fseq))
  colnames(Fratio)<- paste0("Fratio", 1:ncol(Fratio))
  return(Fratio)
}
