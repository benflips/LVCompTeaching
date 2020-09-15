## ---------------------------
##
## Script name: LVFunctions.R
##
## Purpose of script: Functions for running and plotting numerical solutions to LV competition equations
##
## Author: Ben Phillips
##
## Date Created: 2020-09-15
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 
library(deSolve)
## ---------------------------

## load up our functions into memory

## ---------------------------

# The L-V system
  # pars are list r1, r2, a12, a21, K1, K2
LVComp <- function(t, N, pars){
  list(c(
    pars$r1*N[1]*(1-(N[1]+pars$a12*N[2])/pars$K1),
    pars$r2*N[2]*(1-(N[2]+pars$a21*N[1])/pars$K2)
    )
  )
}

# Solve it
  # N0 is a vector of initial N1, N2
  # maxTime is maximum number of generations
  # # pars are list r1, r2, a12, a21, K
LVSolve <- function(N0, maxTime = 100, pars){
  as.data.frame(ode(y = N0, times = 0:maxTime, func = LVComp, parms = pars))
}

# Get critical points for isoclines given parameters
isoPoints <- function(pars){
  iso12x <- c(0, pars$K1, 0, pars$K2/pars$a21)
  iso12y <- c(pars$K1/pars$a12, 0, pars$K2, 0)
  isoID <- c(1, 1, 2, 2)
  data.frame(isoID, iso12x, iso12y)
}

