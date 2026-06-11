#' Count Items Start with Zero
#'
#' The CLCM R package requires that count items start at 0. This helper
#' shifts each specified item so that its minimum observed value is 0.
#' @param dat dataframe containing the item responses
#' @param items vector of item names. These should be the count items that need
#' to start at zero in order for `clcm` function to estimate the model.
#' @return Returns the dataframe `dat` with the columns named in `items`
#' shifted so that the minimum observed value of each is 0.
#' @export
#' @examples
#' dat <- data.frame(Item_1 = c(3, 5, 4, 7),
#'                   Item_2 = c(10, 12, 11, 15))
#' start_with_zero(dat, items = c('Item_1', 'Item_2'))


start_with_zero <- function(dat, items){

    dat.min <- apply(dat[, items], 2, function(x) min(x, na.rm = TRUE) )
    dat.min <- matrix(dat.min, nrow = nrow(dat), ncol = length(dat.min), byrow = TRUE)
    dat[, items] <- dat[, items] - dat.min
    return(dat)

  }
