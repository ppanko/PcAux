### Title:    Create Principal Component Auxiliary Variables
### Author:   Kyle M. Lang & Steven Chesnut
### Created:  2015-SEP-17
### Modified: 2017-MAR-15

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
                        verbose      = 2L,
                        doImputation = TRUE,
                        castData     = !doImputation,
                        control,
                        ...)
{
    quarkData$setCall(match.call(), parent = "createPcAux")
    
    ## Check for problems with the input values:
    if(missing(quarkData)) errFun("noQuarkData")
    if(missing(nComps))    errFun("noNComps")
    if(!simMode)           checkInputs(parent = "createPcAux")
    
    ## Add elements to an extant instance of the QuarkData class:
    quarkData$nComps   <- nComps
    quarkData$forcePmm <- TRUE # Don't give imputation options other than PMM
    quarkData$intMeth  <- as.integer(interactType)
    quarkData$maxPower <- as.integer(maxPolyPow)
    quarkData$simMode  <- simMode
    quarkData$verbose  <- as.integer(verbose)
    
    if(!missCheck(seed)) quarkData$seed <- as.integer(seed)
    
    ## Make sure the control list is fully populated:
    if(!missCheck(control)) quarkData$setControl(x = control)
    
    ## Check for extant moderators when interactType == 1 or 2:
    check <- interactType %in% c(1, 2) & missCheck(quarkData$moderators)
    if(check) {
        quarkData$moderators <- colnames(quarkData$data)
        warnFun("noMods")
    }
    
    ## Re-cast the data if needed
    if(castData) castData(map = quarkData)
    
    if(doImputation) {
        ## Check for and treat any nominal variables that are missing only one
        ## datum:
        singleMissNom <-
            with(quarkData, (nrow(data) - respCounts == 1) &
                            (typeVec == "binary" | typeVec == "nominal")
                 )
        ## KML 2016-NOV-14: Ignore dropped variables
        singleMissNom <- setdiff(names(singleMissNom)[singleMissNom],
                                 quarkData$dropVars[ , 1])
        
        if(length(singleMissNom) > 0) quarkData$fillNomCell(singleMissNom)
    }
    
    ## Compute interactions for use during initial imputation:
    if(quarkData$intMeth == 1) {
        quarkData$computeInteract()
        quarkData$data <- with(quarkData, data.frame(data, interact))
    }
    
    ## Compute polynomials for use during initial imputation:
    if(quarkData$maxPower > 1) {
        quarkData$computePoly()
        quarkData$data <- with(quarkData, data.frame(data, poly))
    }
    
    ## Execute the initial, single imputation:
    if(doImputation) doSingleImputation(map = quarkData)
    
    ## Extract the linear principal component scores:
    doPCA(map = quarkData)
    
    ## Are we constructing seperate non-linear PcAux?
    if(quarkData$intMeth != 1 & quarkData$maxPower > 1) {
        ## Construct and orthogonalize interaction terms:
        quarkData$computeInteract()
        
        ## Extract the nonlinear principal component scores:
        doPCA(map = quarkData)
    }
    quarkData
}# END createPcAux()
