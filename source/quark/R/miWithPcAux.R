### Title:    Conduct Multiple Imputation with PC Auxiliaries
### Author:   Kyle M. Lang & Steven Chesnut
### Created:  2015-SEP-17
### Modified: 2017-MAR-09
### Purpose:  Use the principal component auxiliaries produced by createPcAux()
###           to conduct MI.

### Copyright (C) 2017 Kyle M. Lang
###
### This program is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with this program.  If not, see <http://www.gnu.org/licenses/>.


miWithPcAux <- function(rawData,
                        quarkData,
                        nImps      = 100L,
                        nomVars    = NULL,
                        ordVars    = NULL,
                        idVars     = NULL,
                        dropVars   = "useExtant",
                        nComps     = NULL,
                        compFormat = "list",
                        seed       = NULL,
                        simMode    = FALSE,
                        forcePmm   = FALSE,
                        nProcess   = 1L,
                        verbose    = 2L,
                        control)
{
    quarkData$setCall(match.call(), parent = "miWithPcAux")

    if(missing(rawData))   errFun("noData")
    if(missing(quarkData)) errFun("noQuarkData")
    
    ## Get variable types:
    if(!missCheck(nomVars)) {
        quarkData$nomVars        <- nomVars
        quarkData$dropVars[ , 1] <- setdiff(quarkData$dropVars[ , 1], nomVars)
    }
    if(!missCheck(ordVars)) {
        quarkData$ordVars        <- ordVars
        quarkData$dropVars[ , 1] <- setdiff(quarkData$dropVars[ , 1], ordVars)
    }
    if(!missCheck(idVars)) {
        quarkData$idVars         <- idVars
        quarkData$dropVars[ , 1] <- setdiff(quarkData$dropVars[ , 1], idVars)
    }
    if(length(dropVars) == 1 && dropVars == "useExtant") {
        tmp <- quarkData$dropVars[quarkData$dropVars[ , 2] == "user_defined", ]
        if(class(tmp) != "matrix") tmp <- matrix(tmp, 1, 2)
        quarkData$dropVars <- tmp
    } else if(!missCheck(dropVars)) {
        quarkData$dropVars <- cbind(dropVars, "user_defined")
        quarkData$nomVars  <- setdiff(quarkData$nomVars, dropVars)
        quarkData$ordVars  <- setdiff(quarkData$ordVars, dropVars)
        quarkData$idVars   <- setdiff(quarkData$idVars, dropVars)
    } else {
        quarkData$dropVars <- cbind("NONE_DEFINED", "user_defined")
    }

    ## Check inputs' validity:
    if(!simMode) checkInputs(parent = "miWithPcAux")
    
    ## Combine the principal component auxiliaries with the raw data:
    mergeOut <-
        mergePcAux(quarkData = quarkData, rawData = rawData, nComps = nComps)
    
    ## Populate new fields in the extant QuarkData object:
    quarkData$data       <- mergeOut
    quarkData$nImps      <- as.integer(nImps)
    quarkData$simMode    <- simMode
    quarkData$compFormat <- compFormat
    quarkData$forcePmm   <- forcePmm
    quarkData$verbose    <- as.integer(verbose)
    
    if(!missCheck(seed)) quarkData$seed <- as.integer(seed)
    
    ## Make sure the control list is fully populated:
    if(!missCheck(control)) quarkData$setControl(x = control)
    
    ## Cast the variables to their appropriate types:
    castData(map = quarkData, doingQuark = FALSE)
    
    ## Check and clean the data:
    if(!simMode) cleanData(map = quarkData, doingQuark = FALSE)
    
    ## Check for and treat any single nominal variables that are missing
    ## only one datum
    singleMissNom <- with(quarkData, (nrow(data) - respCounts == 1) &
                                     (typeVec == "binary" | typeVec == "nominal")
                          )
    if(any(singleMissNom))
        quarkData$fillNomCell(colnames(quarkData$data[singleMissNom]))
    
    if(verbose) cat("\nMultiply imputing missing data...\n")
    
    ## Construct a predictor matrix for mice():
    if(verbose) cat("--Constructing predictor matrix...")
    predMat <-
        makePredMatrix(quarkData$data, quarkData$nComps[1], quarkData$nComps[2])
    if(verbose) cat("done.\n")
    
    ## Specify a vector of elementary imputation methods:
    if(verbose) cat("--Creating method vector...")
    quarkData$createMethVec()
    if(verbose) cat("done.\n")
    
    if(forcePmm & verbose) cat("PMM forced by user.\n")
    
    ## Multiply impute the missing data in 'mergedData':
    if(verbose) cat("--Imputing missing values...\n")
    
    if(nProcess == 1) {# Impute in serial
        quarkData$miceObject <- try(
            mice(quarkData$data,
                 m               = nImps,
                 maxit           = 1L,
                 predictorMatrix = predMat,
                 method          = quarkData$methVec,
                 printFlag       = verbose,
                 ridge           = quarkData$miceRidge,
                 seed            = quarkData$seed,
                 nnet.MaxNWts    = quarkData$maxNetWts),
            silent = TRUE)
        
        if(class(quarkData$miceObject) != "try-error") {
            quarkData$data <- "Removed to save resources."
            
            ## Complete the incomplete data sets:
            quarkData$completeMiData()
        } else {
            errFun("miceCrash", map = quarkData)
        }
        
    } else {# Impute in parallel
        myCluster <- makeCluster(nProcess)
        clusterEvalQ(myCluster, library(mice))
        
        quarkData$miDatasets <- parLapply(myCluster,
                                          X       = c(1 : nImps),
                                          fun     = parallelMice,
                                          predMat = predMat,
                                          map     = quarkData)
        
        stopCluster(myCluster)
        
        if(quarkData$compFormat != "list") quarkData$transformMiData()
    }
    
    if(verbose) cat("\n--done.\n")
    if(verbose) cat("Complete.\n")
    
    quarkData
}# END miWithPcAux()
    
