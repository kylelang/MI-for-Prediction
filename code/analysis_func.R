
library(MLmetrics)

                                        #outList <- out[[1]]
                                        #what <- "mi"

calcOutcomes <- function(outList, what, betaTrue = NULL){
                                        #Get values from the complete dataset analysis
    indices <- 1:nrow(outList$comp[[1]]$PI) ### KML
    
    preds_comp  <- lapply(lapply(outList[["comp"]], "[[", x = "PI"), '[', indices)

                                        #preds_comp[[1]] - outList$comp[[1]]$PI[ , 1]
    
    y_val_rbind <- do.call("rbind", lapply(outList[["comp"]], "[[", x = "y"))
    colnames(y_val_rbind) <- paste("val_y", indices, sep = "_")
    
                                        #Get average true y value per prediction over 500 replications
    
    y_true_avg <- colMeans(y_val_rbind)
    
                                        #Get the statistics for the dataset you want to look at : "si", "mi", or "ld"
    
    preds   <- do.call("rbind",
                       lapply(
                           lapply(outList[[what]], "[[", x = "PI"),
                           '[',
                           indices
                       )
                       )
    colnames(preds) <- paste("pred", indices, sep = "_")

    lwr_PI  <- do.call("rbind",
                       lapply(
                           lapply(outList[[what]], "[[", x = "PI"),
                           '[',
                           indices + length(indices)
                       )
                       )
    lowerNames_PI <- colnames(lwr_PI) <- paste("lwr_PI", indices, sep = "_")
    
    upr_PI  <- do.call("rbind",
                       lapply(
                           lapply(outList[[what]], "[[", x = "PI"),
                           '[',
                           indices + length(indices) * 2
                       )
                       )
    upperNames_PI <- colnames(upr_PI) <- paste("upr_PI", indices, sep = "_")
    
    lwr_CI  <- do.call("rbind",
                       lapply(lapply(outList[[what]], "[[", x = "CI"),
                              '[',
                              indices
                              )
                       )
    lowerNames_CI <- colnames(lwr_CI) <- paste("lwr_CI", indices, sep = "_")
    
    upr_CI  <- do.call("rbind",
                       lapply(
                           lapply(outList[[what]], "[[", x = "CI"),
                           '[',
                           indices + length(indices)
                       )
                       )
    upperNames_CI <- colnames(upr_CI) <- paste("upr_CI", indices, sep = "_")

                                        #i <- 6
                                        #preds[i, ] - outList[["mi"]][[i]]$PI[ , 1]    
                                        #lwr_PI[i, ] - outList[["mi"]][[i]]$PI[ , 2]    
                                        #upr_PI[i, ] - outList[["mi"]][[i]]$PI[ , 3]    
                                        #lwr_CI[i, ] - outList[["mi"]][[i]]$CI[ , 1]    
                                        #upr_CI[i, ] - outList[["mi"]][[i]]$CI[ , 2]    
    
    ##Put them all into one list
    
    complete_miss    <- cbind(preds, lwr_PI, upr_PI, lwr_CI, upr_CI, y_val_rbind)

### KML: Why are you greping for these names? Just use the column names you created above.
    
                                        #lowerNames_PI <-
                                        #colnames(complete_miss)[grep("lwr_PI", colnames(complete_miss), ignore = TRUE)]
    
                                        #upperNames_PI <-
                                        #colnames(complete_miss)[grep("upr_PI", colnames(complete_miss), ignore = TRUE)]
    
                                        #lowerNames_CI <-
                                        #colnames(complete_miss)[grep("lwr_CI", colnames(complete_miss), ignore = TRUE)]
    
                                        #upperNames_CI <-
                                        #colnames(complete_miss)[grep("upr_CI", colnames(complete_miss), ignore = TRUE)]
    
                                        #val_y_Names <-
                                        #colnames(complete_miss)[grep("val_y", colnames(complete_miss), ignore = TRUE)]
    
    ##CI coverage
    
    tmp_ci <- apply(X      = complete_miss,
                    MARGIN = 1,
                    FUN    = function(x, y, u, l) x[u] > y & x[l] < y,
                    y      = y_true_avg,
                    u      = upperNames_CI,
                    l      = lowerNames_CI)
    cic <- rowMeans(tmp_ci)
    names(cic) <- paste("CI_cov", indices, sep = "_")
    
    ciw <-colMeans(complete_miss[ , upperNames_CI] - complete_miss[ , lowerNames_CI])
    
    
    ##PI coverage
    
### KML: You don't need the 'val_y_names' subsetting
    
    pic <- colMeans(complete_miss[, upperNames_PI] > y_val_rbind & complete_miss[, lowerNames_PI] < y_val_rbind)
    names(pic) <- paste("PI_cov", indices, sep = "_")
    
    piw <-colMeans(complete_miss[ , upperNames_PI] - complete_miss[ , lowerNames_PI])

                                        #Compute bias/variance related outcome measures
    y_bar <- colMeans(preds)
    
    bias  <- y_bar - y_true_avg
    names(bias) <- paste("bias_pred", indices, sep = "_")
    mcsd    <- apply(preds, 2, sd)
    
    
    prb <- 100 * bias / y_true_avg

    ##MSE calculations
    
    mse_list <- list()
    for (i in 1:nrow(preds))
        mse_list[[i]] <- MSE(y_pred = preds[i, ], y_true = y_val_rbind[i, ])
    
    out <- list(bias = bias,
                y_true_avg = y_true_avg,
                prb = prb, 
                mcsd = mcsd,
                cic = cic,
                ciw = ciw,
                pic = pic,
                piw = piw,
                MSE = mse_list)
    out
}
