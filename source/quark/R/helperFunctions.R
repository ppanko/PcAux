### Title:       Quark Helper Functions
### Author:      Kyle M. Lang
### Created:     2015-AUG-03
### Modified:    2016-OCT-04

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


## Print startup message:
.onAttach <- function(libname, pkgname) {
    version <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                        fields = "Version")
    packageStartupMessage(
        "Loading: ", paste(pkgname, version), ", Copyright (C) 2016 Kyle M. Lang."
    )
    packageStartupMessage(
        pkgname, " comes with ABSOLUTELY NO WARRANTY; execute 'quarkW()' for details."
    )
    packageStartupMessage(
        pkgname, " is beta software. Please report any bugs. Thank You."
    )
}


## Count levels of variables:
countLevels <- function(x)
{
    length( unique( x[!is.na(x)] ) )
}


## Wrapper to suppress all gc() output:
silentGC <- function()
{
    if(.Platform$OS.type == "unix")
        nullFile <- "/dev/null"
    else
        nullFile <- "nul"

    sink(nullFile)
    gc()
    sink()
}# END silentGC()



## Create a set of dummy ID values ensured to be disjoint from
## the observed IDs to use as temporary fill-ins for missing IDs:
createDummyIdValues <- function(x)
{
    varType <- class(x)
    if(varType == "numeric" | varType == "integer") {
        idFills <- 2 * max(x, na.rm = TRUE) + c( 0 : (sum(is.na(x)) - 1) )
    }
    else if(varType == "character") {
        idFills <- paste0( "dummyID", c( 1 : sum(is.na(x) ) ) )
    }
    idFills
}# END createDummyIdValues()



## Calculate various kinds of 'correlation' coefficient:
flexLinearAssoc <- function(varNames, map, checkMat = FALSE)
{
    options(warn = -1)# Suppress warnings

    ## Find the class of the target variables:
    varType <- map$typeVec[varNames]

    if(.Platform$OS.type == "unix")
        nullFile <- "/dev/null"
    else
        nullFile <- "nul"

    sink(nullFile)# Suppress output

    ## Check for pairwise available obvservations
    pairwiseCheck <- mean(
        apply(!is.na(map$data[ , varNames]), 1, all)
    )

    if(pairwiseCheck != 0) {
        ## Compute an appropriate measure of linear association:
        if( all(varType == "continuous") ) {
            ## Pearson's R for two continuous
            corVal <- cor(map$data[ , varNames[1] ],
                          map$data[ , varNames[2] ],
                          method = "pearson",
                          use = "pairwise")
            corType <- "pearson"
        }
        else if( all(varType == "ordinal") ) {
            ## Spearman's Rho for two ordinal
            corVal <- cor(as.numeric(map$data[ , varNames[1] ]),
                          as.numeric(map$data[ , varNames[2] ]),
                          method = "spearman",
                          use = "pairwise")
            corType <- "spearman"
        }
        else if( all(varType == "nominal" | varType == "binary") ) {
            ## Cramer's V for two nominal
            corVal <- assocstats( table(map$data[ , varNames]) )$cramer
            corType <- "cramer"
        }
        else if(all(varType == "ordinal" |
                        varType == "nominal" |
                            varType == "binary") ) {
            ## Cramer's V for nominal and ordinal
            corVal <- assocstats( table(map$data[ , varNames]) )$cramer
            corType <- "cramer"
        }
        else if( all(varType == "continuous" | varType == "ordinal") ) {
            ## Spearman's Rho for continuous and ordinal
            corVal <- cor(as.numeric(map$data[ , varNames[1] ]),
                          as.numeric(map$data[ , varNames[2] ]),
                          method = "spearman",
                          use = "pairwise")
            corType <- "spearman"
        }
        else if(all(varType == "continuous" |
                        varType == "nominal" |
                            varType == "binary") ) {
            ## Itra-class correlation for continuous and nominal
            nomVar <- varNames[varType == "nominal" | varType == "binary"]
            contVar <- varNames[varType == "continuous"]
            corVal <- ICCbare(x = nomVar, y = contVar, data = map$data)
            corType <- "icc"
        }
        else {
            ## NA for unrecognized class pairs
            corVal <- NA
        }
    }
    else {
        ## NA with no pairwise available data
        corVal <- NA
    }

    sink()# Stop suppressing output
    options(warn = 0)# Back to defaults

    outList <- list(value = corVal)
    if(checkMat) {
        outList$corType <- corType
        outList$varTypes <- varType
    }
    outList
}# END flexLinearAssoc()



## Compute several measures of central tendency:
flexCenTen <- function(x)
{
    varType <- class(x)[1]
    if(varType == "numeric" | varType == "integer") {
        ## Mean for continuous variables
        cenVal <- mean(x, na.rm = TRUE)
    }
    else if(varType == "factor" |
            varType == "ordered" |
            varType == "character") {
        ## Mode for nominal and ordinal variables
        tmpTab <- table(x)
        cenVal <- names(tmpTab)[tmpTab == max(tmpTab)]
        if(length(cenVal) > 1) cenVal <- sample(cenVal, 1)
    }
    else {
        cenVal <- NA
    }
    cenVal
}# END flexCenTen()



## Convert factors to dummy codes:
factorToDummy <- function(facVar, labelStem, refLevel = NULL)
{
    ## Remove empty factor levels (KML 2016-JUL-30):
    missLevels <- setdiff(levels(facVar), unique(facVar))
    levels(facVar)[levels(facVar) %in% missLevels] <- NA
    length(levels(facVar))
    
    dummyFrame <- data.frame(
        matrix(0,
               length(facVar),
               length(levels(facVar))
               )
    )                            
    
    if( is.null(refLevel) ) refLevel = ncol(dummyFrame)
    
    for( i in 1 : nrow(dummyFrame) ) {
        if(!is.na(facVar[i])) {
            dummyFrame[i, as.numeric(facVar[i])] <- 1
        }
        else {
          dummyFrame[i, ] <- NA
        }
    }

    outFrame <- data.frame(dummyFrame[ , -refLevel])
    colnames(outFrame) <- paste0(labelStem, "_", levels(facVar)[-refLevel])
    outFrame
}# END factorToDummy()



## Flexibly check for missing arguments:
missCheck <- function(x)
{
    if( missing(x) ) {
        outVal <- TRUE
    } else if( !is.object(x) ) {
        if(length(x) == 0) {
            outVal <- TRUE
        } else if(length(x) == 1) {
            if(is.na(x) | is.null(x) | x == "") {
                outVal <- TRUE
            } else {
                outVal <- FALSE
            }
        } else {
            outVal <- FALSE
        }
    } else {
        outVal <- FALSE
    }
    outVal
}



## Scale data with minimal memory usage:
lowMemScale <- function(inData)
{
    for( i in 1 : ncol(inData) ) {
        inData[ , i] <-
            ( inData[ , i] - mean(inData[ , i]) ) / sd(inData[ , i])
    }
    inData
}



## Do a simple PCA while trying to minimize memory usage:
simplePca <- function(inData, nComps, scale = FALSE)
{
    ## Scale the raw data:
    if(scale) {
        for( i in 1 : ncol(inData) ) {
            inData[ , i] <-
                ( inData[ , i] - mean(inData[ , i]) ) / sd(inData[ , i])
        }
    }

    ## Get the eigen decomposition
    eigenOut <- eigen(cov(inData), symmetric = TRUE)

    ## Due to numerical instability, some eigenvalues stray below zero.
    ## Replace such values with zero:
    eigenOut$values[eigenOut$values < 0.0] <- 0.0

    ## NOTE: Output's labels are chosen for compatability with prcomp()
    list(sdev = sqrt(eigenOut$values), # SD of each component
         x = as.matrix(inData) %*% eigenOut$vectors[ , 1 : nComps]) # PC scores
}# END simplePca()



warnFun <- function(type, map)
{
    ## Select an appropriate warning message:
    warnMessage <-
        switch(type,
               badNoms =
                   paste0("The following variables have ",
                          "been declared as nominal: ",
                          toString(map$probNoms),
                          ", but they take: ",
                          toString(map$levelVec[colnames(map$data) %in%
                                                map$probNoms]),
                          " levels, respectively.\n"),
               badOrds =
                   paste0("The following variables have ",
                          "been declared as ordinal: ",
                          toString(map$probOrds),
                          ", but they take: ",
                          toString(map$levelVec[colnames(map$data) %in%
                                                map$probOrds]),
                          " levels, respectively.\n"),
               badCons =
                   paste0("The following variables have ",
                          "been declared as continuous: ",
                          toString(map$probCons),
                          ", but they only take: ",
                          toString(map$levelVec[colnames(map$data) %in%
                                                map$probCons]),
                          " levels, respectively.\n"),
               highPm =
                   paste0("The following variables have fewer than ",
                          map$minRespCount,
                          " observed responses: ",
                          toString(map$highPmVars),
                          ".\n"),
               emptyVars =
                   paste0("The following variables have ",
                          "no observed responses ",
                          "and have been excluded from ",
                          "the data analysis: ",
                          toString(map$emptyVars),
                          ".\n"),
               quarkConstCols =
                   paste0("The following data columns ",
                          "are constants ",
                          "and have been excluded from ",
                          "the data analysis: ",
                          toString(map$constants),
                          ".\n"),
               romConstCols =
                   paste0("The following data columns ",
                          "are constants: ",
                          toString(map$constants),
                          ".\n Their missing data have been ",
                          "replaced with the appropriate ",
                          "constant value.\n"),
               collin = {
                   tmpDropNames <- unique(map$collinVars$var1)
                   prettyPairs <- toString(
                       apply(map$collinVars, 1,
                             FUN = function(x) {
                                 paste0("{", x[1], ", ", x[2], "}")
                             }
                             )
                   )
                   paste0("The following variable pairs ",
                          "are bivariate collinear: ",
                          prettyPairs,
                          ", so ",
                          toString(tmpDropNames),
                          " will be excluded from ",
                          "further analysis.\n")
               },
               firstImpFail =
                   paste0("First-pass imputation has failed ",
                          "for the following variables: ",
                          toString(map$impFails$firstPass),
                          ".\nI will now attempt to fill their missing ",
                          "data by using predictive mean matching (PMM)."),
               pmmFail =
                   paste0("Predictive mean matching has failed ",
                          "for the following variables: ",
                          toString(map$impFails$pmm),
                          ".\nI will now attempt to fill their missing ",
                          "data by using group-mean substitution."),
               groupMeanFail =
                   paste0("Group-mean substitution has failed ",
                          "for the following variables: ",
                          toString(map$impFails$groupMean),
                          ".\nI will now fill their missing ",
                          "data by using grand-mean substitution."),
               noGroupVars =
                   paste0("No grouping variables were specified, ",
                          "so I will employ grand-mean substitution ",
                          "to treat the following variables: ",
                          toString(map$impFails$pmm),
                          "."),
               dropGroupVars =
                   paste0("All grouping variables have been dropped, ",
                          "so I will employ grand-mean substitution ",
                          "to treat the following variables: ",
                          toString(map$impFails$pmm),
                          "."),
               grandMeanFail =
                   paste0("Grand-mean substitution has failed ",
                          "for the following variables: ",
                          toString(map$impFails$grandMean),
                          ".\nThese variables will be exluded from ",
                          "subsequent analyses."),
               linPcNum = {
                   datCols <- ncol(map$data) - length(map$idVars)
                   nComps <- map$nComps[1]
                   paste0("The number of linear principal component ",
                          "scores that you requested (i.e., ",
                          nComps,
                          ") is greater than the number of remaining ",
                          "columns in your data object (i.e., ",
                          datCols,
                          "). So, nComps[1] will be set to: ",
                          datCols,
                          ".\n")
               },
               nonLinPcNum = {
                   datCols <- ncol(map$data) - length(map$idVars)
                   nComps <- map$nComps[2]
                   paste0("The number of nonlinear principal component ",
                          "scores that you requested (i.e., ",
                          nComps,
                          ") is greater than the number of remaining ",
                          "columns in your data object (i.e., ",
                          datCols,
                          "). So, nComps[2] will be set to: ",
                          datCols,
                          ".\n")
               },
               mergeNoID =
                   paste0("The ID variables defined in the QuarkData object (i.e., ",
                          toString(map$idVars),
                          ") do not exist in the raw data, so the ",
                          "merging was accomplished via naive column-binding.\n",
                          "Please confirm the output object's row alignment."),
               mergeNoID2 =
                   paste0("The ID variables defined in the QuarkData object (i.e., ",
                          toString(map$idVars),
                          ") do not exist in the PcAux data, so the ",
                          "merging was accomplished via naive column-binding.\n",
                          "Please confirm the output object's row alignment."),
               mergeBadID =
                   paste0("None of the potential ID variables (i.e., ",
                          toString(map$idVars),
                          ") are unique row-identifiers, so the ",
                          "merging was accomplished via naive column-binding.\n",
                          "Please confirm the output object's row alignment."),
               miceCrash =
                   paste0("The mice() algorithm has crashed while creating ",
                          "imputation number ",
                          map$impNum,
                          "and returned the following error message:\n",
                          map$miceObj)
               )

    ## Print the warning message
    options(warn = 1) # Print warnings immediately
    warning(warnMessage, call. = FALSE)
    options(warn = 0) # Back to normal
}# END warnFun()



errFun <- function(type, ...)
{
    x <- list(...) # Unpack extra arguments

    ## Select the appropriate error message:
    errMessage <-
        switch(type,
               noData =
                   paste0("Please provide a data object ",
                          "for the rawData argument.\n"),
               badDataType =
                   paste0("Please provide a data frame or ",
                          "matrix for the rawData argument.\n"),
               smallPower =
                   "maxPower must be an integer greater than 1.\n",
               largePower =
                   "Polynomial powers greater than 4 are not supported.\n",
               noLinPc =
                   paste0("You must ",
                          ifelse(x$doingQuark, "extract", "use"),
                          " at least 1 linear auxiliary ",
                          "principal component score. You have requested 0.\n"),
               nonLinOptionClash =
                   paste0("You have requested a non-trivial number of non-linear ",
                          "principal component scores (i.e., ",
                          x$nNonLinear,
                          "), but you have also told me not to compute any ",
                          "interactions or polynomial terms.\nI am confused.\n",
                          "Could you please adjust the values supplied to the ",
                          "'nComps', 'useInteract', and 'usePoly' arguments so ",
                          "that they are consistent?\n"),
               missingVars =
                   paste0("Some of the arguments you've supplied ",
                          "correspond to variables that do ",
                          "not exist in the data object.\n",
                          "The problematic variables are: ",
                          toString(x$varNames[x$checkVec]),
                          ".\n"),
               dropVarOverlap =
                   paste0("The set of variable names supplied ",
                          "for the 'dropVars' argument overlaps ",
                          "with the variable names supplied for other ",
                          "arguments.\nThe problematic variables are: ",
                          toString(unique(x$varNames2[x$checkVec2])),
                          ".\nPlease ensure that 'dropVars' and ",
                          "c('idVars', 'nomVars', 'ordVars', 'groupVars') ",
                          "are disjoint sets.\n"),
               idOverlap = {
                   tmpVec <-
                       ifelse(
                           x$doingQuark,
                           "c('nomVars', 'ordVars', 'groupVars', 'dropVars') ",
                           "c('nomVars', 'ordVars', 'dropVars') "
                       )
                   paste0("The set of variable names supplied ",
                          "for the 'idVars' argument overlaps ",
                          "with the variable names supplied for other ",
                          "arguments.\nThe problematic variables are: ",
                          toString(unique(x$varNames3[x$checkVec3])),
                          ".\nPlease ensure that 'idVars' and ",
                          tmpVec,
                          "are disjoint sets.\n")
               },
               userKill =
                   "Execution stopped by user.\n",
               pmmCrash =
                   paste0("The PMM algorithm has crashed in mice() ",
                          " and returned the following error message:\n",
                          x$map$data),
               miceCrash =
                   paste0("The mice() algorithm has crashed during the ",
                          "initial imputation attempt ",
                          "and returned the following error message:\n",
                          x$map$data),
               badPcaMemLev =
                   paste0("An unrecognized value has been specified ",
                          "for the 'pcaMemLevel' argument (i.e., ",
                          x$map$pcaMemLev,
                          ").\nPlease provide a value of '0' ",
                          "or '1' for this argument."),
               missingNonLinPcAux =
                   paste0("You have requested the use of non-linear ",
                          "principal component scores, but 'quarkData' ",
                          "does not contain any non-linear principal ",
                          "component scores.\n Please adjust your ",
                          "analysis accordingly.\n"),
               linPcAuxVarExp =
                   paste0("The number of available linear ",
                          "component scores (i.e., ",
                          x$quarkData$nComps[1],
                          ") cannot explain the requested ",
                          "proportion of variance (i.e., ",
                          x$varExpLin,
                          ").\nPlease consult the output of: ",
                          "'inspect(quarkData, what = \"rSquared\")' ",
                          "for more details."),
               fewLinPcAux =
                   paste0("The number of linear component scores you ",
                          "requested (i.e., ",
                          x$nLinear,
                          ") is greated than the number of linear component ",
                          "scores available in 'quarkData' (i.e., ",
                          x$quarkData$nComps[1],
                          ").\nPlease adjust your analysis accordingly.\n"),
               nonLinPcAuxVarExp =
                   paste0("The number of available non-linear ",
                          "component scores (i.e., ",
                          x$quarkData$nComps[2],
                          ") cannot explain the requested ",
                          "proportion of variance (i.e., ",
                          x$varExpNonLin,
                          ").\nPlease consult the output of: ",
                          "'inspect(quarkData, what = \"rSquared\")' ",
                          "for more details."),
               fewNonLinPcAux =
                   paste0("The number of non-linear component scores you ",
                          "requested (i.e., ",
                          x$nNonLinear,
                          ") is greated than the number of non-linear component ",
                          "scores available in 'quarkData' (i.e., ",
                          x$quarkData$nComps[2],
                          ").\nPlease adjust your analysis accordingly.\n")
               )# CLOSE switch()

    stop(errMessage, call. = FALSE)
}# END errFun()




## Unpack the extra arguments to doSingleImputation()
## to construct a list of debugging flags:
createSkipFlags <- function(...)
{
    args <- list(...)

    if(!is.null(args$skipFirstPass)) {
        skipFirstPass <- args$skipFirstPass
    } else {
        skipFirstPass <- FALSE
    }

    if(!is.null(args$skipPmm)) {
        if(args$skipPmm)
            skipPmm <- skipFirstPass <- TRUE
        else
            skipPmm <- FALSE
    } else {
        skipPmm <- FALSE
    }

    if(!is.null(args$skipGroupMean)) {
        if(args$skipGroupMean)
            skipGroupMean <-
                skipPmm <- skipFirstPass <- TRUE
        else
            skipGroupMean <- FALSE
    } else {
        skipGroupMean <- FALSE
    }

    if(!is.null(args$skipGrandMean)) {
        if(args$skipGrandMean)
            skipGrandMean <- skipGroupMean <-
                skipPmm <- skipFirstPass <- TRUE
        else
            skipGrandMean <- FALSE
    } else {
        skipGrandMean <- FALSE
    }

    skipList <- list(firstPass = skipFirstPass,
                     pmm       = skipPmm,
                     groupMean = skipGroupMean,
                     grandMean = skipGrandMean)
    skipList
}



## Setup the doSingleImputation calling environment
## for imputation checking procedures:
prepImpChecks <- function()
{
    env <- parent.frame()
    env$frozenData <- env$map$data
}

## Undo an imputation for the purposes of functionality
## of fall-back imputation methods:
undoImputation <- function()
{
    env <- parent.frame()
    env$map$data <- env$frozenData
}

makePredMat <- function(map)
{
    options(warn = -1)
    ## Construct a predictor matrix for mice():
    predMat <- quickpred(map$data,
                         mincor = map$minPredCor,
                         exclude = map$idVars)

    ## Make sure we have fewer predictors than rows:
    badPredFlag <- rowSums(predMat) > (nrow(map$data) - 1)

    if(any(badPredFlag)) {# Some models have P > (N - 1)
        badVars <- colnames(map$data)[badPredFlag]
        for(v in badVars) {
            ## Get names of potential predictors:
            candidates <- setdiff(colnames(map$data),
                                  c(map$idVars, map$dropVars[ , 1], v)
                                  )
            ## Find the bivariate linear associations with the target:
            tmpList <-
                lapply(candidates,
                       FUN = function(x, y, map) {
                           flexLinearAssoc(varNames = c(x, y),
                                           map = map)$value
                       },
                       y = v,
                       map = map)
            tmpVec <- do.call(c, tmpList)
            ## Select the strongest N - 1 candidates to use as predictors:
            predNames <-
                candidates[order(abs(tmpVec))][1 : (nrow(map$data) - 1)]
            ## Modify the predictor matrix accordingly:
            predMat[v, ] <- 0
            predMat[v, colnames(predMat) %in% predNames] <- 1
        }
    }
    options(warn = 0)
    predMat
}# END makePredMat()

#####                       #####
##### Work on the following #####
#####                       #####

                                        #impList <- romOut
                                        #impFrame <- impList[[1]]

                                        #computeFMI <- function(impList)
                                        #{
                                        #  meanFrame <- do.call("rbind",
                                        #                       lapply(impList,
                                        #                              FUN = function(impFrame) {
                                        #                                unlist(lapply(impFrame, flexCenTen))
                                        #                              })
                                        #                       )
                                        #
                                        #  bVec <- apply(meanFrame, 2, var)
                                        #
                                        #  wVec <- colMeans(
                                        #            do.call("rbind",
                                        #                    lapply(impList,
                                        #                           FUN = function(x) {
                                        #                             sd(x) / sqrt(length(x))
                                        #                           })
                                        #                    )
                                        #            )
                                        #
                                        #  fmiVec <- bVec / (wVev + bVec)
                                        #}

