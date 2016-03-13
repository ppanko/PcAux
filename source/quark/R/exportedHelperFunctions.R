### Title:    Exported Quark Helper Functions
### Author:   Kyle M. Lang
### Created:  2015-OCT-29
### Modified: 2016-FEB-18

### Copyright (C) 2016 Kyle M. Lang
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


## Merge the Principal Component Auxiliaries with the
## raw data from which they were created:
mergePcAux <- function(quarkData,
                       rawData,
                       nLinear = NULL,
                       nNonLinear = NULL,
                       varExpLin = NULL,
                       varExpNonLin = NULL,
                       verbose = TRUE)
{
    idVars <- quarkData$idVars
    
    nlPcCheck <- quarkData$nComps[2] > 0
    
    if(!is.null(varExpNonLin)) {
        nlPcRequested <- varExpNonLin > 0.0
    } else if(!is.null(nNonLinear)) {
        nlPcRequested <- nNonLinear > 0
    } else {
        nlPcRequested <- nlPcCheck
    }

    if(!nlPcCheck & nlPcRequested) errFun("missingNonLinPcAux")
        
    useNonLin <- nlPcCheck & nlPcRequested
    
    ## Get names for the desired number of principal component scores:
    if(!is.null(varExpLin)) {
        if(varExpLin == 0.0) {# Requesting no linear components?
            errFun("noLinPc", doingQuark = FALSE)
        } else {# Requesting linPcAux > 0?
            critVal <- sum(quarkData$rSquared$lin < varExpLin) + 1
            if(critVal <= quarkData$nComps[1]) {# Requesting a legal number?
                linPcNames <- paste0("linPC", c(1 : critVal))
                
                if(verbose) {
                    print(
                        paste0("NOTE: ",
                               critVal,
                               " component ",
                               ifelse(critVal > 1,
                                      "scores explain ",
                                      "score explains "),
                               round(quarkData$rSquared$lin[critVal], 3),
                               " of the variance in 'rawData.'")
                    )
                }
                
            } else {# Requesting too many components?
                errFun("linPcAuxVarExp",
                       quarkData = quarkData,
                       varExpLin = varExpLin)    
            }
        }
        
    } else if(!is.null(nLinear)) {
        if(nLinear == 0) {
            errFun("noLinPc", doingQuark = FALSE)
        } else if(nLinear <= quarkData$nComps[1]) {
            linPcNames <- paste0("linPC", c(1 : nLinear))
        } else {
            errFun("fewLinPcAux",
                   quarkData = quarkData,
                   nLinear = nLinear)
        }
    } else {
        linPcNames <-
            setdiff(colnames(quarkData$pcAux$lin), idVars)
    }
    
    if(useNonLin) {
        if(!is.null(varExpNonLin)) {
            critVal <- sum(quarkData$rSquared$nonLin < varExpNonLin) + 1
            
            if(critVal <= quarkData$nComps[2]) {
                nonLinPcNames <- paste0("nonLinPC", c(1 : critVal))
                
                if(verbose) {
                    print(
                        paste0("NOTE: ",
                               critVal,
                               " component ",
                               ifelse(critVal > 1,
                                      "scores explain ",
                                      "score explains "),
                               round(quarkData$rSquared$nonLin[critVal], 3),
                               " of the variance in the nonlinear ",
                               "expansion of 'rawData.'")
                    )
                }
                
            } else {
                errFun("nonLinPcAuxVarExp",
                       quarkData = quarkData,
                       varExpNonLin = varExpNonLin)
            }
            
        } else if(!is.null(nNonLinear)) {
            if(nNonLinear <= quarkData$nComps[2]) {
                nonLinPcNames <- paste0("nonLinPC", c(1 : nNonLinear))
                
            } else {
                errFun("fewNonLinPcAux",
                       quarkData = quarkData,
                       nNonLinear = nNonLinear)
            }
            
        } else {
            nonLinPcNames <-
                setdiff(colnames(quarkData$pcAux$nonLin), idVars)
        }
    }# CLOSE if(useNonLin)
    
    ## Check for an ID variable in rawData:
    idCheck2 <- idVars %in% colnames(rawData)

    ## Check for an ID variable in pcAux:
    idCheck3 <- idVars %in% colnames(quarkData$pcAux$lin) &
        idVars %in% colnames(quarkData$pcAux$nonLin)
    
    if( !any(idCheck2) ) {# No ID in rawData
        warnFun("mergeNoID", quarkData)        
        matchVec <- c(1 : nrow(rawData))
    } else if ( !any(idCheck3) ) {# No ID in pcAux
        warnFun("mergeNoID2", quarkData)
        matchVec <- c(1 : nrow(rawData))
    } else {# At least one matching ID variable
        idVars <- idVars[idCheck3 & idCheck2]
        
        ## Check that ID variables are unique row-identifiers:
        uniqueIdCheck <-
            do.call("c",
                    lapply(
                        as.data.frame(
                            quarkData$pcAux$lin[ , idVars]
                        ),
                        FUN = function(x) { length( unique(x) ) == length(x) }
                    )
                    )
        
        if( any(uniqueIdCheck) ) {# At least one viable ID variable
            
            ## Arbitrarily select an acceptable ID variable to use for matching:
            useId <- idVars[uniqueIdCheck][1]
            dataId <- rawData[ , useId]
            
            ## Temporarily cast factor-valued raw data IDs as character:
            factorCheck <-
                class(dataId) == "factor" | class(dataId) == "ordered"
            if(factorCheck) dataId <- as.character(dataId)
            
            idMissPat <- is.na(dataId)
            
            ## Fill missing ID values with dummy levels from the Quark object:
            if( any(idMissPat) )
                dataId[idMissPat] <- quarkData$idFills[[useId]]
            
            ## Match ID variables in raw data and Quark object:
            matchVec <- match(dataId, quarkData$pcAux$lin[ , useId])
            
        } else {# No viable ID variables
            warnFun("mergeBadID", quarkData)            
            matchVec <- c(1 : nrow(rawData))
        }
    }
    
    ## Pool the PC Auxiliaries and raw data and return the merged data object:
    if(useNonLin) {
        outData <- data.frame(rawData,
                              data.frame(
                                  quarkData$pcAux$lin[ , linPcNames],
                                  quarkData$pcAux$nonLin[ , nonLinPcNames]
                              )[matchVec, ]
                              )
        colnames(outData) <- c(colnames(rawData), linPcNames, nonLinPcNames)
    } else {
        outData <-
            data.frame(rawData, quarkData$pcAux$lin[matchVec, linPcNames])
        colnames(outData) <- c(colnames(rawData), linPcNames)
    }

    nLin <- length(linPcNames)
    nNonLin <- ifelse(useNonLin, length(nonLinPcNames), 0)
    
    list(data = outData,
         nLinear = as.integer(nLin),
         nNonLinear = as.integer(nNonLin)
         )
}# END mergePcAux()




## Make a predictor matrix suitable for use by mice() that designated
## subset of the principle component auxiliaries produced by quark()
## as the imputation model predictors:
makePredMatrix <- function(mergedData,
                           nLinear = NULL,
                           nNonLinear = NULL)
{
    if(is.null(nLinear))
        nLinear <- length(grep("linPC", colnames(mergedData)))
    
    if(is.null(nNonLinear))
        nNonLinear <- length(grep("nonLinPC", colnames(mergedData)))
    
    if(nNonLinear > 0) {
        predVars <- c(paste0("linPC", c(1 : nLinear)),
                      paste0("nonLinPC", c(1 : nNonLinear))
                      )
    } else {
        predVars <- paste0("linPC", c(1 : nLinear))
    }
    
    predMat <- matrix(0, ncol(mergedData), ncol(mergedData))
    colnames(predMat) <- rownames(predMat) <- colnames(mergedData)
    predMat[ , predVars] <- 1
    
    compFlag <- colSums(is.na(mergedData)) == 0
    predMat[compFlag, ] <- 0
    
    predMat
}# END makePredMatrix()



## Wrapper function to give S3/S4-like access to fields:
inspect <- function(object, what)
{
    object$field(what)
}



## Wrapper function to return the imputed data sets:
getImpData <- function(quarkData)
{
    quarkData$miDatasets
}
