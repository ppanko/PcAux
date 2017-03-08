### Title:    Create Principal Component Auxiliary Variables
### Author:   Kyle M. Lang & Steven Chesnut
### Created:  2015-SEP-17
### Modified: 2017-MAR-08

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


createPcAux <- function(quarkData,
                        nComps,
                        interactType = 1L,
                        maxPolyPow   = 3L,
                        simMode      = FALSE,
                        seed         = NULL,
                        forcePmm     = FALSE,
                        verbose      = 2L,
                        doImputation = TRUE,
                        castData     = !doImputation,
                        control,
                        ...)
{
    quarkData$setCall(match.call(), parent = "createPcAux")
    
    ## Check for problems with the input values:
    if(!simMode) checkInputs(parent = "createPcAux")

    ## Add elements to an extant instance of the QuarkData class:
    quarkData$nComps   <- nComps
    quarkData$forcePmm <- forcePmm
    quarkData$intMeth  <- as.integer(interactType)
    quarkData$maxPower <- as.integer(maxPolyPow)
    quarkData$simMode  <- simMode
    quarkData$verbose  <- as.integer(verbose)
    
    if(!missCheck(seed)) quarkData$seed <- as.integer(seed)
    
    ## Make sure the control list is fully populated:
    if(!missCheck(control)) quarkData$setControl(x = control)
    
    ## Populate some important elements of the QuarkData object:
    if(quarkData$maxPower > 1) 
        for(pp in 2 : quarkData$maxPower) {
            powerVal <- switch(pp - 1, "square", "cube", "quad")
            quarkData$setPoly(x = data.frame(NULL), power = powerVal)
        }

    ## Check for extant moderators when interactType == 1:
    check <- interactType == 1 & length(quarkData$moderators$raw) == 0
    if(check) {
        quarkData$moderators$raw <- colnames(quarkData$data)
        warnFun("noMods")
    }
    
    ## Re-cast the data if needed
    if(castData) castData(map = quarkData)
    
    if(doImputation) {
        ## Check for and treat any single nominal variables that are missing
        ## only one datum
        singleMissNom <-
            with(quarkData, (nrow(data) - respCounts == 1) &
                            (typeVec == "binary" | typeVec == "nominal")
                 )
        ## KML 2016-NOV-14: Ignore dropped variables
        singleMissNom <- setdiff(names(singleMissNom)[singleMissNom],
                                 quarkData$dropVars[ , 1])
        
        if(length(singleMissNom) > 0) quarkData$fillNomCell(singleMissNom)
        
        ## NOTE: '...' pass hidden debugging flags that allow developers to
        ## check the functionality of the fall-back imputation methods.
        doSingleImputation(map = quarkData, ...)
    }
    
    ## Extract the linear principal component scores:
    doPCA(map = quarkData)
    
    if(nComps[2] != 0) {# Construct seperate non-linear PcAux?
        ## Construct and orthogonalize nonlinear terms:
        quarkData$computeNonLin()
        
        ## Extract the nonlinear principal component scores:
        doPCA(map = quarkData)
    }
    quarkData
}# END createPcAux()
