### Title:    Exported Quark Helper Functions
### Author:   Kyle M. Lang
### Created:  2015-OCT-29
### Modified: 2017-MAR-06

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


### Print the (lack of) warranty information:
quarkW <- function() {
    cat('
  15. Disclaimer of Warranty.

  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

  16. Limitation of Liability.

  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.

  17. Interpretation of Sections 15 and 16.

  If the disclaimer of warranty and limitation of liability provided
above cannot be given local legal effect according to their terms,
reviewing courts shall apply local law that most closely approximates
an absolute waiver of all civil liability in connection with the
Program, unless a warranty or assumption of liability accompanies a
copy of the Program in return for a fee.
')
}# END quarkW()



### Merge the Principal Component Auxiliaries with the raw data from which they
### were created:
mergePcAux <- function(quarkData,
                       rawData,
                       nComps  = NULL,
                       verbose = TRUE)
{
    idVars <- quarkData$idVars
    
    ## If no explicit number of components is defined, use all available:
    if(missCheck(nComps)) {
        nComps <- quarkData$nComps
    } else {# Ascertain the number of components to use
        tmp         <- grep("max", nComps, ignore.case = TRUE)
        varExp[tmp] <- 1.0
        
        if(any(is.na(varExp))) {
            tmp         <- nComps < 1
            varExp[tmp] <- nComps[tmp]
        }
        
        if(!is.na(varExp[1])) nComps[1] <-
                                  sum(quarkData$rSquared$lin < varExp[1]) + 1
        if(!is.na(varExp[2])) nComps[2] <-
                                  sum(quarkData$rSquared$nonLin < varExp[2]) + 1

        ## Must use at least 1 linear PcAux
        check <- nComps[1] == 0
        if(check) errFun("noLinPc", doingQuark = FALSE)

        ## Make sure non-linear PcAux are available, if requested
        check <- quarkData$nComps[2] == 0 & nComps[2] > 0
        if(check) errFun("missingNonLinPc")
        
        ## Requesting a legal number of linear components?
        check <- nComps[1] <= quarkData$nComps[1]
        if(check) {
            if(verbose) {# How much variance is explained?
                print(
                    paste0("NOTE: ",
                           nComps[1],
                           " component ",
                           ifelse(nComps[1] > 1,
                                  "scores explain ",
                                  "score explains "),
                           round(quarkData$rSquared$lin[nComps[1]], 3),
                           " of the variance in 'rawData.'")
                )
            }
        } else {
            if(is.na(varExp[1])) # Argument -> Component counts
                errFun("fewLinPcAux",
                       quarkData = quarkData,
                       nComps    = nComps)
            else # Argument -> Variance explained
                errFun("linPcAuxVarExp",
                       quarkData = quarkData,
                       varExp    = varExp)    
        }
        
        ## Requesting a legal number of non-linear components?
        check <- nComps[2] > 0 & nComps[2] <= quarkData$nComps[2]
        if(check) {
            if(verbose) {# How much variance is explained?
                print(
                    paste0("NOTE: ",
                           nComps[2],
                           " component ",
                           ifelse(nComps[2] > 1,
                                  "scores explain ",
                                  "score explains "),
                           round(quarkData$rSquared$nonLin[nComps[2]], 3),
                           " of the variance in the nonlinear ",
                           "expansion of 'rawData.'")
                )
            }
        } else {
            if(is.na(varExp[1])) # Argument -> Component counts
                errFun("fewNonLinPcAux",
                       quarkData = quarkData,
                       nComps    = nComps)
            else # Argument -> Variance explained
                errFun("nonLinPcAuxVarExp",
                       quarkData = quarkData,
                       varExp    = varExp)    
        }
    }# CLOSE if(missCheck(nComps))

    badId <- FALSE
    
    ## Check for shared ID variables:
    check <- idVars %in% colnames(rawData) &
        idVars %in% colnames(quarkData$pcAux$lin) &
        idVars %in% colnames(quarkData$pcAux$nonLin)
    
    if(!any(check)) {
        warnFun("mergeNoID")
        badId <- TRUE
    } else {
        idVars <- idVars[check]
        
        ## Check that ID variables are unique row-identifiers:
        check <-
            unlist(
                lapply(as.data.frame(quarkData$pcAux$lin[ , idVars]),
                       function(x) length(unique(x)) == length(x)
                       )
            )
        if(any(check)) {# At least one viable ID variable
            
            ## Arbitrarily select an acceptable ID variable to use for matching:
            useId    <- idVars[check][1]
            dataId   <- rawData[ , useId]
            extraIds <- setdiff(idVars, useId)
            
            ## Temporarily cast factor-valued raw data IDs as character:
            check <- class(dataId) == "factor" | class(dataId) == "ordered"
            if(check) dataId <- as.character(dataId)
            
            idMissPat <- is.na(dataId)
            
            ## Fill missing ID values with dummy levels from the Quark object:
            if(any(idMissPat))
                ## KML 2016-JUL-31: Check class of 'idFills' to avoid crashes
                ## with one incomplete ID
                if(is.list(quarkData$idFills))
                    dataId[idMissPat] <- quarkData$idFills[[useId]]
                else
                    dataId[idMissPat] <- quarkData$idFills
        } else {# No viable ID variables
            warnFun("mergeBadID", quarkData)
            badId <- TRUE
        }
    }
    
    ## Merge the PcAux scores onto the raw data:
    if(badId) {
        outData <- data.frame(rawData, quarkData$pcAux)
    } else      {
        tmp <- merge(quarkData$pcAux$lin, quarkData$pcAux$nonLin)
        outData <-
            merge(rawData, tmp[ , setdiff(colnames(tmp), extraIds)], by = useId)
    }
    outData
}# END mergePcAux()




### Make a predictor matrix suitable for use by mice() that designated subset of
### the principle component auxiliaries produced by quark() as the imputation
### model predictors:
makePredMatrix <- function(mergedData,
                           nLinear    = NULL,
                           nNonLinear = NULL)
{
    if(missCheck(nLinear))
        nLinear <- length(grep("linPC", colnames(mergedData)))
    
    if(missCheck(nNonLinear))
        nNonLinear <- length(grep("nonLinPC", colnames(mergedData)))
    
    if(nNonLinear > 0) {
        predVars <- c(paste0("linPC", c(1 : nLinear)),
                      paste0("nonLinPC", c(1 : nNonLinear))
                      )
    } else {
        predVars <- paste0("linPC", c(1 : nLinear))
    }
    
    predMat              <- matrix(0, ncol(mergedData), ncol(mergedData))
    colnames(predMat)    <- rownames(predMat) <- colnames(mergedData)
    predMat[ , predVars] <- 1
    
    compFlag            <- colSums(is.na(mergedData)) == 0
    predMat[compFlag, ] <- 0
    
    predMat
}# END makePredMatrix()



## Wrapper function to give S3/S4-like access to fields:
inspect <- function(object, what) object$field(what)



## Wrapper function to return the imputed data sets:
getImpData <- function(quarkData) quarkData$miDatasets

