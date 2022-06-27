rm(list = ls(all = TRUE))


                                        #resDir  <- "C:/Users/Bas/Documents/Studie/Master/Thesis/Simulation/Results/"
                                        #outDir  <- "C:/Users/vanve/Documents/Studie/Applied_Data_Science_Utrecht/Thesis project/Simulation/Output2/"
resDir <- "../../results/"
outDir <- "../../output/test3/"

nReps <- 6

r2  <- c(0.1, 0.3, 0.5)                # R-Squared
impmethod <- c("cart", "norm.boot", "lasso.select.norm", "pmm", "rf")
missingtype <- c("MCAR", "MAR","MNAR", "pMNAR")

conds <- expand.grid(r2 = r2, impmethod = impmethod, missingtype = missingtype, stringsAsFactors = FALSE)

out  <- list()
reps <- rep(0, nrow(conds))
for(i in 1 : nrow(conds)) {

    ## Create a condition tag to label output objects:
    tag1 <- with(conds[i, ],
                 paste0( "_rs", 100 * r2)
                 )
    tag2 <- with(conds[i, ],
                 paste0(tag1, "_missingtype_", missingtype)
                 )
    tag3 <- with(conds[i, ],
                 paste0(tag2, "_impmethod_", impmethod)
                 )

    out0 <- list()
    for(rp in 1 : nReps) {
        compName <- paste0(outDir,
                           "compOut", tag1,
                           "_rep", rp,
                           ".rds")
        ldName   <- paste0(outDir,
                           "ldOut", tag2,
                           "_rep", rp,
                           ".rds")
        siName   <- paste0(outDir,
                           "siOut", tag3,
                           "_rep", rp,
                           ".rds")
        miName  <- paste0(outDir,
                          "miOut", tag3,
                          "_rep", rp,
                          ".rds")

        test1 <-
            file.exists(compName) &
            file.exists(ldName) &
            file.exists(siName) &
            file.exists(miName)

        if(test1) {
            reps[i]         <- reps[i] + 1
            out0$conds      <- conds[i, ]
            out0$comp[[rp]] <- readRDS(compName)
            out0$ld[[rp]]   <- readRDS(ldName)
            out0$si[[rp]]   <- readRDS(siName)
            out0$mi[[rp]]   <- readRDS(miName)
        }
    }# END for(rp in 1 : nReps)

    out[[i]] <- out0
}# END for(i in 1 : nrow(conds)


source("analysis_func.R")
## test met loop
res <- list()
for(i in 1 : nrow(conds)) {
    tmp <- list()
    for(j in c("si", "mi", "ld", "comp"))
        tmp[[j]] <- calcOutcomes(outList = out[[i]], what = j)
                                        # res[[i]] <- unlist(tmp)
    res[[i]] <- tmp
}


