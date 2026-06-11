#' Two-Stage Estimation of the CLCM
#'
#' Treat item parameters as fixed to fit a longitudinal CLCM. The model
#' can have any number of timepoints. The only limitation is the estimation
#' of the posteriors - item parameters are fixed.
#' The CLCM R package requires that count items start at 0.
#' @param dat dataframe containing the new item responses you want to fit the
#' model to.
#' @param clcm.mod a CLCM model that you already fit (Stage 1). This will
#' have the item parameter estimates, item names, item type, and Q-matrix.
#' @param lprior.t1 the log-prior used to initialize the first timepoint in
#' `dat`; an N by L matrix, where L is the number of latent classes. Use
#' `compute_lprior()` on the Stage 1 posteriors to obtain this.
#' @return Returns a dataframe containing the item responses in `dat`
#' together with the posterior probabilities of latent class membership
#' computed at each timepoint under the fixed Stage 1 item parameters.
#' @export
#' @examples
#' \donttest{
#' set.seed(3112021)
#' sim.dat <- simulate_clcm(N = 200,
#'                          number.timepoints = 2,
#'                          item.type = rep('Ordinal', 5),
#'                          categories.j = rep(4, 5),
#'                          lc.prop = list('Time_1' = c(0.5, 0.5),
#'                                         'Time_2' = c(0.5, 0.5)) )
#'
#' # Stage 1: fit the model to the first timepoint only
#' dat.t1 <- sim.dat$dat[sim.dat$dat$Time == 'Time_1', ]
#' mod1 <- clcm(dat = dat.t1,
#'              item.type = sim.dat$item.type,
#'              item.names = sim.dat$item.names,
#'              Q = sim.dat$Q,
#'              verbose = FALSE)
#'
#' # Stage 2: fixed item parameters, estimate posteriors at timepoint 2
#' lprior1 <- compute_lprior(post = mod1$dat[, mod1$post.names],
#'                           type = 'standard_EB',
#'                           K = mod1$K)
#' dat.t2 <- sim.dat$dat[sim.dat$dat$Time == 'Time_2', ]
#' post2 <- clcm_2stage(dat = dat.t2, clcm.mod = mod1, lprior.t1 = lprior1)
#' head(post2)
#' }



clcm_2stage <- function(dat, clcm.mod, lprior.t1){

  # Pass a CLCM model that you already fit (Stage 1)
  # Pass a dataframe with the subsequent timepoints

  post.all <- vector() #initialize this, will consist of everything

  lprior <- lprior.t1

  TT <- unique(dat$Time)

  for (tt in TT) {

    tmp <- which(dat[, 'Time'] == tt)
    dat.tmp <- dat[tmp, ]

    # Compute Posterior at Timepoint tt:
    mod <- clcm(dat = dat.tmp,
                initial.lprior = lprior,
                max.it = 0,
                Q = clcm.mod$Q,
                sv = clcm.mod$item.param,
                item.type = clcm.mod$item.type,
                item.names = clcm.mod$item.names,
                verbose = FALSE)


    # Rbind the posteriors - note that mod$dat contains everything including the posterior
    post.all <- rbind.data.frame(post.all, mod$dat)

    # Update prior:
    lprior <- compute_lprior(post = mod$dat[, mod$post.names],
                             type = 'standard_EB',
                             K = mod$K)


  }# end loop over timepoints


  return(post.all)# This should re-combine everything once again

} # end function
