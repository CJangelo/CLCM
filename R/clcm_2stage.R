#' Two-Stage Estimation of the CLCM
#'
#' Treat item parameters as fixed to fit a longitudinal CLCM. The model
#' can have any number of timepoints. The only limitation is the estimation
#' of the posteriors - item parameters are fixed.
#' The CLCM R package requires that count items start at 0
#' @param dat dataframe containing the new item responses you want to fit the
#' model to.
#' @param clcm.mod a CLCM model that you already fit (Stage 1). This will
#' have the item parameter estimates, item names, item type, and Q-matrix.
#' @param lprior this is the log-prior for timepoint 2. N by L matrix.
#' @export
#'
#'



clcm_2stage <- function(dat, clcm.mod, lprior.t1){

  # Pass a CLCM model that you already fit (Stage 1)
  # Pass a dataframe with the subsequent timepoints

  post.all <- vector() #initialize this, will consist of everything

  lprior <- lprior.t1
  #print(head(lprior))
  # lprior <- CLCM::compute_lprior(post = clcm.mod$dat[, clcm.mod$post.names],
  #                                type = 'standard_EB',
  #                                K = clcm.mod$K)

  TT <- unique(dat$Time)

  for (tt in TT) {

    tmp <- which(dat[, 'Time'] == tt)
    dat.tmp <- dat[tmp, ]

    # Compute Posterior at Timepoint tt:
    mod <- CLCM::clcm(dat = dat.tmp,
                      initial.lprior = lprior,
                      max.it = 0,
                      Q = clcm.mod$Q,
                      sv = clcm.mod$item.param,
                      item.type = clcm.mod$item.type,
                      item.names = clcm.mod$item.names )


    # Rbind the posteriors - note that mod$dat contains everything including the posterior
    post.all <- rbind.data.frame(post.all, mod$dat)

    # Update prior:
    lprior <- CLCM::compute_lprior(post = mod$dat[, mod$post.names],
                                   type = 'standard_EB',
                                   K = mod$K)


  }# end loop over timepoints


  return(post.all)# This should re-combine everything once again

} # end function

