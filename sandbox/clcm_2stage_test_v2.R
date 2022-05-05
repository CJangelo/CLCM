
# TODO: include latent regerssion option with the lprio

###
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

lpriort1 <- CLCM::compute_lprior(post = clcm.mod$dat[, clcm.mod$post.names],
                                 type = 'standard_EB',
                                 K = clcm.mod$K)

pa <- CLCM::clcm_2stage(dat = dat, clcm.mod = clcm.mod, lprior.t1 = lpriort1)
xtabs(~ lca + Time, data = pa)

