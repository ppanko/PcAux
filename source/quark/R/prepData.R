### Title:    Data Preperation
### Author:   Kyle M. Lang
### Created:  2016-JAN-19
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


prepData <- function(rawData,
                     nomVars = NULL,
                     ordVars = NULL,
                     idVars = NULL,
                     dropVars = NULL,
                     groupVars = NULL,
                     simMode = FALSE,
                     mySeed = 235711L,
                     useParallel = FALSE,
                     nProcess = 1L,
                     verbose = !simMode,
                     control,
                     ...)
{
    ## Check for problems with the input values:
    if(!simMode) checkInputs(parent = "prepData")
    
    if(missCheck(dropVars)) {
        dropVars <- "NONE_DEFINED"
    } else {
        dropVars <- dropVars
    }
    
    ## Initialize a new instance of the QuarkData class
    ## to store all of the data and metadata for this run:
    quarkData <- QuarkData(data         = rawData,
                           dropVars     = dropVars,
                           simMode      = simMode,
                           seed         = as.integer(mySeed),
                           useParallel  = useParallel,
                           nProcess     = nProcess,
                           verbose      = verbose)

    quarkData$setCall(match.call(), parent = "prepData")
    
    ## Make sure the control list is fully populated:
    conDefault <- list(miceIters    = 10L,
                       miceRidge    = 1e-5,
                       collinThresh = 0.95,
                       minRespCount = as.integer(
                           floor(0.05 * nrow(rawData))
                       ),
                       minPredCor   = 0.1,
                       maxNetWts    = 10000L,
                       nomMaxLev    = 10L,
                       ordMaxLev    = 10L,
                       conMinLev    = 10L,
                       nGVarCats    = 3L)
    
    if(missCheck(control)) {
        quarkData$setControl(conDefault)
    } else {
        for( i in names(conDefault) ) {
            if( i %in% names(control) ) {
                conDefault[[i]] <- control[[i]]
            }
        }
        quarkData$setControl(conDefault)
    }
    rm(conDefault)

    ## Check for special variable arguments and fill the
    ## appropriate slots in the quarkData object:
    if( missCheck(idVars) ) {
        quarkData$addVars(x = c(1 : nrow(quarkData$data)), names = "idVar")
        quarkData$idVars <- "idVar"
        quarkData$dummyId <- TRUE
    } else {
        quarkData$idVars <- idVars
    }
    ## Do we actually need to create an ID variable if none exist?
    if( !missCheck(groupVars) ) {
        quarkData$groupVars <- groupVars
    }
    if( !missCheck(nomVars) ) {
        quarkData$nomVars <- nomVars
    }
    if( !missCheck(ordVars) ) {
        quarkData$ordVars <- ordVars
    }

### Pre-process the data ###

    ## Cast the variables to their declared types:
    castData(map = quarkData)

    if(!simMode) {# Don't clean data in simMode
        ## Remove constant and empty columns:
        cleanData(map = quarkData)

        ## Find any (bivariate) collinear variables:
        findCollin(map = quarkData)
    }

    quarkData
}# END prepData()
