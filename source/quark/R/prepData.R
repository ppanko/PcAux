### Title:        Data Preperation
### Author:       Kyle M. Lang
### Contributors: Byung Jung
### Created:      2016-JAN-19
### Modified:     2017-MAR-07

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


prepData <- function(rawData,
                     moderators = NULL,
                     nomVars    = NULL,
                     ordVars    = NULL,
                     idVars     = NULL,
                     dropVars   = NULL,
                     groupVars  = NULL,
                     simMode    = FALSE,
                     mySeed     = 235711L,
                     nProcess   = 1L,
                     verbose    = !simMode,
                     control,
                     ...)
{
    ## Check for problems with the input values:
    if(!simMode) checkInputs(parent = "prepData")
    
    if(missCheck(dropVars)) dropVars <- "NONE_DEFINED"
            
    ## Initialize a new instance of the QuarkData class
    ## to store all of the data and metadata for this run:
    quarkData <- QuarkData(data     = rawData,
                           dropVars = dropVars,
                           simMode  = simMode,
                           seed     = as.integer(mySeed),
                           nProcess = as.integer(nProcess),
                           verbose  = verbose)

    quarkData$setCall(match.call(), parent = "prepData")
    
    ## Make sure the control list is fully populated:
    if(!missCheck(control)) quarkData$setControl(x = control)
    
    ## Check for special variable arguments and fill the appropriate slots in the
    ## quarkData object:
    if(!missCheck(idVars)) {
        quarkData$idVars <- idVars
        quarkData$idCols <- as.data.frame(quarkData$data[ , idVars])
        quarkData$data   <-
            quarkData$data[ , setdiff(colnames(quarkData$data), idVars)]
    }
    if(!missCheck(groupVars))  quarkData$groupVars <- groupVars
    if(!missCheck(nomVars))    quarkData$nomVars   <- nomVars
    if(!missCheck(ordVars))    quarkData$ordVars   <- ordVars
    if(!missCheck(moderators))
        quarkData$moderators <-
            list(raw   = moderators,
                 coded = setdiff(moderators, quarkData$nomVars)
                 )
    
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
