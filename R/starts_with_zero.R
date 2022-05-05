#' Count Items Start with Zero
#'
#' The CLCM R package requires that count items start at 0
#' @param dat dataframe containing the item responses
#' @param items vector of item names. These should be the count items that need
#' to start at zero in order for `clcm` function to estimate the model.
#' @export
#'
#'

start_with_zero <- function(dat, items){

    dat.min <- apply(dat[, items], 2, function(x) min(x, na.rm = T) )
    dat.min <- matrix(dat.min, nrow = nrow(dat), ncol = length(dat.min), byrow = T)
    dat[, items] <- dat[, items] - dat.min
    return(dat)

  }
