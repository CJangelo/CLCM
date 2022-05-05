rm(list = ls())
gc()

library(CLCM)
r.dir <- "C:/Users/ciaconangelo/OneDrive - OPEN Health/Documents/Amgen/"
setwd(r.dir)
# Read in the model with the item parameters from 05
#mod05 <- readRDS(file = 'C:/Users/ciaconangelo/OneDrive - OPEN Health/Documents/Amgen/DATA/lcm_mod_2005.rds')

# read in data
source('C:/Users/ciaconangelo/OneDrive - OPEN Health/Documents/Amgen/CODE/data_mgmt_all_years.R')
dat <- data_mgmt_all_years(ICHD.avail = T, comp.LCM.items = T, all.years = T)
# Use complete case data, computing things is a mess otherwise


# Latent Class Models -----
## 1 Dimension, 2 Latent Classes ----
K <- 1
item.names <- grep('Pain|midas|Nausea|Phono|Photo|Number', colnames(dat), value = T)
item.type <- c(rep('Ordinal', 7), rep('Neg_Binom', 6))
Q <- matrix(1, nrow = length(item.names), ncol = K)
# 2005 only:
clcm.mod <- CLCM::clcm(dat = dat[dat$year == '2005', ], Q=Q,
                  item.type = item.type,
                  item.names = item.names )


# You'll need to pass these:
dat <- dat[dat$year != '2005', ]
dat$Time <- dat$year


# FUNCTION
clcm_2stage <- function(dat, clcm.mod){

  # Pass a CLCM model that you already fit (Stage 1)
  # Pass a dataframe with the subsequent timepoints

  post.all <- vector() #initialize this, will consist of everything

  lprior <- CLCM::compute_lprior(post = clcm.mod$dat[, clcm.mod$post.names],
                                 type = 'standard_EB',
                                 K = clcm.mod$K)

  TT <- unique(dat$Time)
  #TT <- TT[-1]
  #TT <- length(tp)

  #if (is.null(clcm.mod)) { stop('pass a model fit using CLCM')}

  #tmp <- any(lapply(c(lprior.t1, item.param,  Q, item.type,  item.names), is.null))

  #if (tmp) { stop('pass the necessary parameters for 2 stage estimation' }
    tt <- '2006'
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

xtabs(~ lca + Time, data = post.all)

all( unlist(mod$item.param) == unlist(clcm.mod$item.param) )
# > xtabs(~ lca + Time, data = post.all)
#    Time
# lca 2006 2007 2008 2009
#   1  530  577  600  611
#   2  189  142  119  108
