#' @title Change numeric-like levels of a factor to a numeric
#'
#' @description
#' \code{fvalues4levels} changes levels of a factor to numeric variable for plotting of the PRC diagram
#'
#' @param  object  a data frame or tibble.
#' @param fctr name of a factor in object (character or symbol)
#' @return  a numeric named vector (names are levels)
#' @details
#' If condition is null, the focal
#'
#' @example demo/PRC_pyrifos_fval.R
#' @export
fvalues4levels <- function(object, fctr){
  # fctr is name (symbol) or character string
  if (!is.character(fctr))fctr <- as.character(substitute(fctr))
  if (is.character(object[[fctr]]) )object[[fctr]] <- factor(object[[fctr]])
  if (is.factor(object[[fctr]]) ) {
    lvs <- levels(object[[fctr]])
    mx <- max(nchar(lvs))
    mn <- min(nchar(lvs))
    vals <- suppressWarnings(as.numeric(lvs))
    while (any(is.na(vals)) && mn >1){
      # drop the first charactor
      lvs <- substr(lvs, 2, mx)
      mx <- mx -1; mn <- mn - 1
    }
    vals <- suppressWarnings(as.numeric(lvs))
    if (!is.numeric(vals)|| any(is.na(vals)) ) vals <- seq_along(levels(object[[fctr]])) # numbers 1:nlev

  } else {
    if(!is.numeric(object[[fctr]]))
      stop(paste( fctr  ," is not a factor in object argument of fvalues4levels", sep = ""))
    else {
      vals <- as.numeric(levels(factor(object[[fctr]])))
      warning(paste( fctr  ," is not a factor in object argument of fvalues4levels", sep = ""))}
  }
  names(vals) <- levels(object[[fctr]])
  return(vals)
}
