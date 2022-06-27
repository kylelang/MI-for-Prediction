source("sim_func.R")
source("miss_func.R")

                                        #hinstall.packages("rlecuyer", repos = "http://cloud.r-project.org")

library(parallel)
library(rlecuyer)
library(mvtnorm)
library(mice)
library(mitools)
library(dplyr)
library(magrittr)

                                        #applyLib(c("rlecuyer", "mvtnorm", "mice", "mitools"))

## Parse command line arguments:
                                        #outDir  <- "C:/Users/vanve/Documents/Studie/Applied_Data_Science_Utrecht/Thesis project/Simulation/Output3/"
outDir <- "../../output/test3/"

## Define levels of variable simulation parameters:
r2  <- c(0.1, 0.3, 0.5)                # R-Squared
impmethod <- c("cart", "norm.boot", "lasso.select.norm", "pmm", "rf")
missingtype <- c("MCAR", "MAR","MNAR", "pMNAR")

conds <- expand.grid(r2 = r2, impmethod = impmethod, missingtype = missingtype, stringsAsFactors = FALSE)

## Define the fixed simulation parameters:
parms <- list()
parms$verbose    <- FALSE
parms$miceIters  <- 5
parms$outDir     <- outDir
parms$incompVars <- c("y", "x1", "x2", "x3", "x4")
parms$MARpred    <- c("x5", "x6", "x7", "x8")
parms$MNARpred   <- c("y", "x1", "x2", "x3", "x4")
parms$missType   <- c("high", "low", "center", "tails", "low")
parms$coefs      <- matrix(c(1.0, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33))
parms$varNames   <- c("y", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
parms$model      <- as.formula("y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10")
parms$mySeed     <- 235711
parms$nStreams   <- 500
parms$nObs       <- 550
parms$nImps      <- 5
parms$covX       <- 0.1
parms$nVal       <- 50

if(FALSE) {#####################################################################

cl <- makeCluster(3)

## Export stuff to the cluster workers:
clusterEvalQ(cl, c(library(rlecuyer), library(mvtnorm), library(mice), library(mitools), source("sim_func.R"), source("miss_func.R")))

## Run the simulation in parallel:

# filename <- file.choose()
# test2 <- readRDS(filename)

it <- seq(1, 6, 1)

                                        #incomplete <- c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 49, 50, 51, 52, 52, 53, 54, 55, 56, 57, 58, 67, 68, 69, 70 , 71, 72, 73, 74, 75, 76, 77, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154)

                                        #system.time(
    parLapply(cl = cl, it, fun = doRep, parms = parms, conds = conds)
                                        #)
stopCluster(cl)

# system.time(
#   mclapply(iterations, FUN = doRep, parms = parms, conds = conds, mc.cores = 6)
# )

}###############################################################################
