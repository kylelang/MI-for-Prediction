### Title:    Simple Simulation Demo
### Author:   Kyle M. Lang
### Created:  2022-05-09
### Modified: 2022-05-09

rm(list = ls(all = TRUE))

library(dplyr)    # For pipes
library(ggplot2)  # For plotting
library(parallel) # For parallel processing

set.seed(235711)


###-Computational kernel-----------------------------------------------------###

## Define design parameters:
n <- 100
m <- 5
s <- 5

## Sample data:
x <- rnorm(n, m, s)

## Compute deviance:
mean(x) - m


###-Evaluate bias for one set of design parameters---------------------------###

## Define number of Monte Carlo replications:
reps <- 500

x <- rep(NA, reps)
for(r in 1:reps)
    x[r] <- rnorm(n, m, s) %>% mean()

## Compute bias:
mean(x) - m


###-Evaluate Bias across a grid of design parameters-------------------------###

## Define vectors of design parameters:
n <- seq(10, 100, 10)
m <- 1:5
s <- 1:5

## Cross all conditions in a full-factorial design matrix:
conds <- expand.grid(n = n, m = m, s = s)

x <- list()
for(r in 1:reps) {
    out <- rep(NA, nrow(conds))
    for(i in 1:nrow(conds))
        out[i] <- rnorm(conds[i, "n"], conds[i, "m"], conds[i, "s"]) %>% mean()
    x[[r]] <- out
}

## Compute bias:
bias <- colMeans(do.call(rbind, x)) - conds[ , "m"]

## Visualize results (badly):
result <- cbind(conds, bias)

ggplot(result, aes(y = bias, x = n, color = factor(m))) +
    geom_line() +
    geom_abline(slope = 0, intercept = 0) +
    facet_grid(cols = vars(s))


###-Same as above but using functions----------------------------------------###

## Define a function to run computations for one set of design parameters:
runCell <- function(cond) {
    rnorm(cond[ , "n"], cond[ , "m"], cond[ , "s"]) %>% mean()
}

## Define a function to do all computation for a single replication:
doRep <- function(rp, conds) {
    out <- rep(NA, nrow(conds))
    for(i in 1:nrow(conds))
        out[[i]] <- runCell(conds[i, ])

    out
}

## Run the simulation:
x <- lapply(1:reps, doRep, conds = conds)

## Compute bias:
bias <- colMeans(do.call(rbind, x)) - conds[ , "m"]

## Visualize the results (even more badly):
result <- cbind(conds, bias)

ggplot(result, aes(y = bias, x = n, color = factor(m), shape = factor(s))) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0)


###-Same as above but using parallel processing------------------------------###

## Create a cluster across 4 processors:
cl <- makeCluster(4)

## Export stuff to the cluster workers:
clusterExport(cl, "runCell")
clusterEvalQ(cl, library(dplyr))

## Run the simulation in parallel:
x <- parLapply(cl = cl, X = 1:reps, fun = doRep, conds = conds)

## Kill the cluster:
stopCluster(cl)

## Compute bias:
bias <- colMeans(do.call(rbind, x)) - conds[ , "m"]

## Visualize the results (now stupidly poorly):
result <- cbind(conds, bias)

ggplot(result, aes(y = bias, x = n, color = factor(m), linetype = factor(s))) +
    geom_line() +
    geom_abline(slope = 0, intercept = 0)

