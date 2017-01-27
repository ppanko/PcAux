### Title:       Quark Subroutines
### Author:      Kyle M. Lang & Stephen Chesnut
### Created:     2015-JUL-27
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


checkInputs <- function(parent)
{

    ## Get access to objects defined in quark():
    env <- parent.frame()
    if(env$verbose) cat("\nChecking inputs' validity...\n")

    if(parent == "prepData") {
        ## Check the data object:
        if( missCheck(env$rawData) ) {
            errFun("noData")
        } else {
            ## Make sure the data object is a data.frame:
            if( !is.data.frame(env$rawData) ) {
                if( is.matrix(env$rawData) ) {
                    env$rawData <- as.data.frame(env$rawData)
                } else {
                    errFun("badDataType")
                }
            }
        }
        ## Check the existance of all designated variables:
        varNames <- with(env, c(idVars, nomVars, ordVars, groupVars, dropVars))
        checkVec <- !varNames %in% colnames(env$rawData)
        if(any(checkVec)) {
            errFun("missingVars",
                   varNames = varNames,
                   checkVec = checkVec)
        }
        ## Check for a non-empty intersection between
        ## 'dropVars' and the other arguments:
        varNames2 <- with(env, c(idVars, nomVars, ordVars, groupVars))
        checkVec2 <- varNames2 %in% env$dropVars
        if(any(checkVec2))
            errFun("dropVarOverlap",
                   varNames2 = varNames2,
                   checkVec2 = checkVec2)

        ## Check for a non-empty intersection between
        ## 'idVars' and the other arguments:
        varNames3 <- with(env, c(nomVars, ordVars, groupVars, dropVars))
        checkVec3 <- varNames3 %in% env$idVars
        if(any(checkVec3))
            errFun("idOverlap",
                   varNames3 = varNames3,
                   checkVec3 = checkVec3,
                   doingQuark = TRUE)
    }

    if(parent == "quark") {
        ## Check the polynomial specification:
        if(env$usePoly) {
            if(env$maxPower < 2)
                errFun("smallPower")
            else if(env$maxPower > 4)
                errFun("largePower")
        }

        ## Check for non-zero linear component counts:
        if(env$nComps[1] == 0) errFun("noLinPc", doingQuark = TRUE)

        ## Check for disagreement between nComps and usePoly/useInteract:
        checkVal <- !env$useInteract & !env$usePoly & env$nComps[2] > 0
        if(checkVal) errFun("nonLinOptionClash", nNonLinear = env$nComps[2])
    }

    if(parent == "rom") {
        ## Check the existance of all designated variables:
        varNames <- with(env, c(idVars, nomVars, ordVars, dropVars))
        varNames <- setdiff(varNames, "NONE_DEFINED")
        checkVec <- !varNames %in% colnames(env$rawData)
        if(any(checkVec))
            errFun("missingVars",
                   varNames = varNames,
                   checkVec = checkVec)

        ## Check for a non-empty intersection between
        ## 'dropVars' and the other arguments:
        varNames2 <- with(env, c(idVars, nomVars, ordVars))
        checkVec2 <- varNames2 %in% env$dropVars
        if(any(checkVec2))
            errFun("dropVarOverlap",
                   varNames2 = varNames2,
                   checkVec2 = checkVec2)

        ## Check for a non-empty intersection between
        ## 'idVars' and the other arguments:
        varNames3 <- with(env, c(nomVars, ordVars, dropVars))
        checkVec3 <- varNames3 %in% env$idVars
        if(any(checkVec3))
            errFun("idOverlap",
                   varNames3 = varNames3,
                   checkVec3 = checkVec3,
                   doingQuark = FALSE)
    }
    if(env$verbose) cat("Complete.\n")
}# END checkInputs()




## Check input formatting and cast variables to declared types:
castData <- function(map, doingQuark = TRUE)
{
    if(map$verbose) cat("\nChecking data and information provided...\n")

    nVars <- ncol(map$data)

    if(map$verbose) cat("--Examining data...")
    ## Count variable levels:
    map$countVarLevels()

    ## Store the initial percent missing:
    map$countResponses(initialPm = TRUE)
    if(map$verbose) cat("done.\n")

    ## Flag variable types:
    if(map$verbose) cat("--Typing data...")
    map$typeData()

    ## If any ID variables are factors, cast them as character objects:
    if(doingQuark) map$idToCharacter()
    if(map$verbose) cat("done.\n")

    ## Cast all variables to the appropriate measurement level:
    if(map$verbose) cat("--Casting data...")
    map$castData()
    if(map$verbose) cat("done.\n")

    confirmTypes <- !map$simMode
    if(confirmTypes) {
        map$checkTypes()

        if(length(map$probNoms) > 0) {# Any suspicious nominal variables?
            warnFun("badNoms", map)

            userAnswer <-
                readline("Do you want to continue the analysis? (y/N) ")
            ansCheck <- grep("y|yes", userAnswer, ignore.case = TRUE)

            if(length(ansCheck) == 0) {
                errFun("userKill")
            } else {
                cat("\nAs you wish.\n")
            }
        }

        if(length(map$probOrds) > 0) {# Any suspicious ordinal variables?
            warnFun("badOrds", map)

            userAnswer <-
                readline("Do you want to continue the analysis? (y/N) ")
            ansCheck <- grep("y|yes", userAnswer, ignore.case = TRUE)

            if(length(ansCheck) == 0) {
                errFun("userKill")
            } else {
                cat("\nAs you wish.\n")
            }
        }

        if(length(map$probCons) > 0) {# Any suspicious continuous variables?
            warnFun("badCons", map)

            userAnswer <-
                readline("Do you want to continue the analysis? (y/N) ")
            ansCheck <- grep("y|yes", userAnswer, ignore.case = TRUE)

            if(length(ansCheck) == 0) {
                errFun("userKill")
            } else {
                cat("\nAs you wish.\n")
            }
        }

    }# CLOSE if(confirmTypes)

    if(map$verbose) cat("Complete.\n")
    silentGC()
}# END castData()




## Find and (possibly) remove problematic data columns
## i.e., variables with few or no observations and constants:
cleanData <- function(map, doingQuark = TRUE)
{
    if(map$verbose)
        cat("\nFinding and addressing problematic data columns...\n")

    if(doingQuark) {
        ## Check for missing data on ID variables:
        missIdCounts <- switch(ifelse(length(map$idVars) == 1, 1, 2),
                               sum( is.na(map$data[ , map$idVars]) ),
                               colSums( is.na(map$data[ , map$idVars]) )
                               )
        missIds <- map$idVars[missIdCounts > 0]

        ## If there are any missing IDs, fill them with dummy values:
        if(length(missIds) > 1) {# More than 1 incomplete ID
            map$idFills <- lapply(map$data[ , missIds],
                                  FUN = createDummyIdValues)

            ## Fill missing IDs with their dummy values
            for(i in missIds)
                map$data[ , i][is.na(map$data[ , i])] <- map$idFills[[i]]

        } else if(length(missIds) == 1) {# Only 1 incomplete ID
            map$idFills <- createDummyIdValues(map$data[ , missIds])
            map$data[ , missIds][is.na(map$data[ , missIds])] <- map$idFills
        }

        rm(missIdCounts)
    }# CLOSE if(doingQuark)

    ## Find each variable's number of observations:
    map$countResponses()

    ## Flag empty variables:
    if(map$verbose) cat("--Checking for empty columns...")
    haveEmptyVars <- map$findEmptyVars(remove = doingQuark)
    if(map$verbose) cat("done.\n")

    ## Flag constant columns:
    if(map$verbose) cat("--Checking for constant columns...")
    haveConstCols <- map$findConstCols(doingQuark = doingQuark)
    if(map$verbose) cat("done.\n")

    ## Flag variables with few responses:
    if(map$verbose) cat("--Checking for high PM...")
    haveHighPmVars <- map$findHighPmVars()
    if(map$verbose) cat("done.\n")

    if(haveHighPmVars) {# Any low-response variables?
        warnFun("highPm", map)
        userAnswer <-
            readline("Would you like to remove them from the analysis? (Y/n) ")
        ansCheck <- grep("n|no", userAnswer, ignore.case = TRUE)

        if(length(ansCheck) == 0) {# Remove the low-response variables
            map$removeVars(x = map$highPmVars, reason = "low_resp_rate")
        }
        rm(userAnswer)
        rm(ansCheck)
    }

    if(haveEmptyVars) warnFun("emptyVars", map)

    if(haveConstCols) warnFun(ifelse(doingQuark,
                                     "quarkConstCols",
                                     "romConstCols"), map)

    if(map$verbose) cat("Complete.\n")
    silentGC()
}# END cleanData()




## Flag variables with perfect bivariate correlations (within some epsilon):
# revise usePaarallel
findCollin <- function(map)
{
    if(map$verbose) cat("\nExamining data for collinear relationships...\n")

    ## Get all unique variable pairings:
    varPairs <- NULL

    tmpVarNames <- setdiff(colnames(map$data), map$idVars)
    varPairs<-data.frame(t(combn(tmpVarNames,2)),stringsAsFactors = F)
    ##If not using any parallel process
    if(!map$useParallel)
      linAssocFrame <- data.frame(varPairs, 
                                  unlist(apply(varPairs, 
                                               1,
                                               FUN = flexLinearAssoc, 
                                               map = map)),
                                  stringsAsFactors = FALSE
                                  )
    else
    {
      myCluster <- makeCluster(map$nProcess)
      clusterEvalQ(myCluster, library(mice))
      linAssocFrame <- data.frame(varPairs, 
                                  unlist(parApply(myCluster, 
                                                  varPairs, 
                                                  1, 
                                                  FUN = flexLinearAssoc, 
                                                  map = map)),
                                  stringsAsFactors = FALSE
                                  )
      stopCluster(myCluster)
    }
    colnames(linAssocFrame) <- c("var1", "var2", "coef")

    collinFlag <- !is.na(linAssocFrame$coef) &
        abs(linAssocFrame$coef) > map$collinThresh

    if( any(collinFlag) ) {
        ## Update the data object by removing the collinear variables:
        map$cleanCollinVars(linAssocFrame[collinFlag, ])
        warnFun("collin", map)
    }

    if(map$verbose) cat("Complete.\n")

    ## Clean up:
    rm(collinFlag)
    rm(linAssocFrame)
    rm(tmpVarNames)
    rm(varPairs)
    silentGC()
}# END findCollin()




## Do the initial single imputation:
doSingleImputation <- function(map, ...)
{
    if(map$verbose) cat("\nDoing initial, single imputation...\n")

    extraArgs <- list(...)
    ## Prepare for debugging checks when extra arguments are supplied
    if(length(extraArgs) > 0) prepImpChecks()
    skipList <- createSkipFlags(...)
    skipFirstPass <- map$forcePmm | skipList$firstPass

    if(!skipFirstPass) {
        ## Construct a design matrix of predictors:
        if(map$verbose) cat("--Constructing predictor matrix...")
        predMat <- makePredMat(map = map)
        if(map$verbose) cat("done.\n")

        ## Specify a vector of elementary imputation methods:
        if(map$verbose) cat("--Creating method vector...")
        map$createMethVec()
        if(map$verbose) cat("done.\n")

        ## Initially fill-in the data with a single imputation:
        if(map$verbose) cat("--Filling missing values...")
        map$data <- try(
            mice(map$data,
                 maxit = map$miceIters,
                 m = 1,
                 predictorMatrix = predMat,
                 method = map$methVec,
                 printFlag = FALSE,
                 seed = map$seed,
                 MaxNWts = map$maxNetWts,
                 ridge = map$miceRidge),
            silent = TRUE)
        if(map$verbose) cat("done.\n")

        rm(predMat)
    }# CLOSE if(!skipFirstPass)

    if(class(map$data) != "try-error") {# mice() didn't crash
        ## Fill missing values with the imputations
        if(!skipFirstPass) map$data <- complete(map$data)

        ## Check for any remaining missing data:
        ## NOTE: map$respCounts now contains counts of missing data
        map$countResponses(countMissing = TRUE)

### If there are any more missing data, try to fill
### them through more robust imputation schemes:

        if( any(map$respCounts > 0) ) {# Any more missing data?

            ## Store names of un-imputed variables:
            if(map$forcePmm) badImps <- "Skipped by User"
            else badImps <- colnames(map$data)[map$respCounts > 0]

            map$updateImpFails(x = badImps, type = "firstPass")

            if(map$forcePmm) {
                if(map$verbose) cat("PMM forced by user.\n")
            } else {
                warnFun("firstImpFail", map)
            }

            if(skipFirstPass) {
                if(map$verbose) cat("--Constructing predictor matrix...")
                predMat <- makePredMat(map = map)
                if(map$verbose) cat("done.\n")
            }

            ## Create a new method vector:
            if(map$verbose) cat("--Creating method vector...")
            if(map$forcePmm) {
                map$createMethVec()
            } else {
                map$methVec <- rep("", ncol(map$data))
                map$setMethVec(x = "pmm",
                               index = map$respCounts > 0)
            }
            if(map$verbose) cat("done.\n")

            ## Try mice() with PMM:
            if(map$verbose) cat("--Filling missing values...")
            map$data <- try(
                mice(map$data,
                     maxit = map$miceIters,
                     m = 1,
                     predictorMatrix = predMat,
                     method = map$methVec,
                     printFlag = FALSE,
                     seed = map$seed,
                     MaxNWts = map$maxNetWts, # KML 2016-OCT-04: Adding maximum
                     ridge = map$miceRidge),  # network weights and ridge options
                silent = TRUE)
            if(map$verbose) cat("done.\n")

            rm(predMat)

            if(class(map$data) != "try-error") {# mice() didn't crash
                map$data <- complete(map$data)# Fill the missing values

                ## Undo the PMM-based imputation to check mean substitution:
                if(skipList$pmm) undoImputation()

                ## Check again for any remaining missing data:
                map$countResponses(countMissing = TRUE)

                if( any(map$respCounts > 0) ) {# Any more missing data?
                    ## Store the names of un-imputed variables:
                    map$updateImpFails(colnames(map$data)[map$respCounts > 0],
                                       type = "pmm")
                    warnFun("pmmFail", map)

                    ## Try Group-Mean / Global-Mean Substitution:
                    meanSubstitute(map, skipList = skipList)
                }# CLOSE if( any(map$respCounts > 0) )

            } else {# mice:pmm() has crashed
                errFun("pmmCrash", map = map)
            }

        } else {# All is well :)
            if(map$verbose)
                cat("All variable successfully imputed in first pass.\n")
        } # CLOSE if( any(map$respCounts > 0) )

    } else {# First-pass mice() has crashed
        errFun("miceCrash", map = map)
    }

    if(map$verbose) cat("Complete.\n")
    silentGC()
}# END doSingleImputation()



## Implement group-mean substitution:
doGroupMeanSub <- function(map)
{
    ## Construct the grouping patterns:
    map$createPatterns()

    for(j in 1 : 2) {
        if(j == 2) {
            ## If the initial group-mean substition isn't fully
            ## successful reverse the order of the grouping
            ## variables and recreate the patterns
            map$groupVars <- rev(map$groupVars)
            map$createPatterns()
        }

        for( i in 1 : length(map$patterns) ) {
            ## Find the unique grouping patterns
            patLevels <- unique(map$patterns[[i]])

            ## Fill the missing data with approprite group means:
            if(sum(map$respCounts > 0) > 1) {
                map$data[ , map$respCounts > 0] <-
                    data.frame(
                        lapply(map$data[ , map$respCounts > 0],
                               FUN = fillWithGroupMean,
                               pat = map$patterns[[i]],
                               patLevs = patLevels)
                    )
            } else {
                map$data[ , map$respCounts > 0] <-
                    fillWithGroupMean(map$data[ , map$respCounts > 0],
                                      pat = map$patterns[[i]],
                                      patLevs = patLevels)
            }

            map$countResponses(countMissing = TRUE)
            if( all(map$respCounts == 0) ) return(0)

            ## Re-create the patterns to incorporate
            ## the grouping variables imputed values
            map$createPatterns()
        }
    }
}# END doGroupMeanSub()




## Fill a variable's missing values with appropriate group-means:
fillWithGroupMean <- function(v, pat, patLevs) {
    for( k in 1 : length(patLevs) ) {
        subData <- subset(v, pat == patLevs[k])
        if( all(is.na(subData)) ) {
            tmp <- NA
        } else {
            tmp <- flexCenTen(subData)
        }

        ## With multiple modes, break ties randomly:
        groupMean <- ifelse(length(tmp) > 1,
                            sample(tmp, size = 1),
                            tmp)

        ## Make sure the group mean is non-missing and finite:
        badMean <- is.na(groupMean) | is.nan(groupMean) |
            is.infinite(groupMean) | is.null(groupMean)

        if(!badMean)
            v[pat == patLevs[k] & is.na(v)] <- groupMean
    }
    v
}# END fillWithGroupMean()



doGrandMeanSub <- function(map)
{
    missCols <- map$respCounts > 0

    if(sum(missCols) == 1) {
        map$data[ , missCols][is.na(map$data[ , missCols])] <-
            flexCenTen(map$data[ , missCols])
    } else {
        map$data[ , missCols] <-
            do.call(data.frame,
                    lapply(map$data[ , missCols],
                           FUN = function(x) {
                               x[is.na(x)] <- flexCenTen(x)
                               x
                           })
                    )
    }
}# END doGrandMeanSub()




## Do the various flavors of mean substitution:
meanSubstitute <- function(map, skipList)
{
    if( !missCheck(map$groupVars) ) {# We have grouping variables
        ## Make sure we don't try to use dropped grouping variables:
        map$groupVars <- setdiff(map$groupVars, map$dropVars[ , 1])

        if( missCheck(map$groupVars) ) {# All groupVars have been dropped
            warnFun("dropGroupVars", map)

            if(map$verbose) cat("--Filling missing values...")
            doGrandMeanSub(map)
            if(map$verbose) cat("done.\n")
        } else {
            ## Try group-mean substitution:
            if(map$verbose) cat("--Filling missing values...")
            doGroupMeanSub(map)
            if(map$verbose) cat("done.\n")

            ## Undo the group-mean imputation to check grand-mean imputaiton:
            if(skipList$groupMean) {
                map$data <- parent.frame()$frozenData
                map$countResponses(countMissing = TRUE)
            }

            if( any(map$respCounts > 0) ) {# Still have missing?
                ## If all else fails, do global-mean substitution
                map$updateImpFails(colnames(map$data)[map$respCounts > 0],
                                   "groupMean")
                warnFun("groupMeanFail", map)

                if(map$verbose) cat("--Filling missing values...")
                doGrandMeanSub(map)
                if(map$verbose) cat("done.\n")

                ## Undo the grand-mean imputation to check failure state:
                if(skipList$grandMean)
                    map$data <- parent.frame()$frozenData
            }
        }
    } else {# We don't have grouping variables
        warnFun("noGroupVars", map)
        doGrandMeanSub(map)
    }# CLOSE if( !is.null(map$groupVar) )

    ## Do a final check for remaining missing values:
    map$countResponses(countMissing = TRUE)

    if( any(map$respCounts > 0) ) {
        ## If any missingness remains exclude the incomplete columns:
        map$updateImpFails(colnames(map$data)[map$respCounts > 0],
                           type = "grandMean")
        warnFun("grandMeanFail", map)

        map$removeVars(x = colnames(map$data)[map$respCounts > 0],
                       reason = "imp_fail")
    }# CLOSE if( any(map$respCounts > 0) )

    silentGC()
}# END meanSubstitute()




doPCA <- function(map)
{
    ## Are we extracting linear or nonlinear PC scores?
    if(length(map$pcAux$lin) == 0) {
        if(map$verbose)
            cat("\nCalculating linear principal component scores...\n")

        linVal <- "lin"
        nComps <- map$nComps[1]

        if(!map$simMode) {
            ## Make sure the number of PC scores we want is less than
            ## the number of columns in our data object:
            datCols <- ncol(map$data) - length(map$idVars)
            if(nComps > datCols) {
                warnFun("linPcNum", map)
                nComps <- map$nComps[1] <- datCols
            }
        }

        if( !all(map$nomVars == "") ) {
            ## Find the nominal variables:
            nomVars <- colnames(map$data)[colnames(map$data) %in% map$nomVars]

            if(length(nomVars) > 0) {
                ## Dummy code the nominal variables:
                dummyNomVars <-
                    do.call("data.frame",
                            lapply(nomVars,
                                   FUN = function(x, map) {
                                       factorToDummy(map$data[ , x], x)
                                   },
                                   map = map)
                            )

                ## Store the dummy variables' names for later:
                map$dummyVars <- colnames(dummyNomVars)

                ## Replace the factor representations of the nominal
                ## variables with their dummy-coded versions:
                map$data <-
                    data.frame(map$data[ , setdiff(colnames(map$data), nomVars)],
                               dummyNomVars)

                rm(nomVars)
                rm(dummyNomVars)
            }
        }

        if( !all(map$ordVars == "") ) {
            ## Find ordinal variables that are still on the data set:
            ordVars <- colnames(map$data)[colnames(map$data) %in% map$ordVars]

            ## Cast the ordinal variables as numeric:
            if(length(ordVars) > 1) {
                map$data[ , ordVars] <-
                    data.frame(lapply(map$data[ , ordVars], FUN = as.numeric))
            } else if(length(ordVars) == 1) {
                map$data[ , ordVars] <- as.numeric(map$data[ , ordVars])
            }
        }

        ## Note the variables we need to scale down below:
        scaleNames <- setdiff(colnames(map$data), map$idVars)

    } else {# We already have linear component scores
        if(map$verbose)
            cat("\nCalculating nonlinear principal component scores...\n")

        linVal <- "nonLin"
        nComps <- map$nComps[2]

        ## Redefine the data object:
        map$data <- map$data[ , map$idVars]
        if(map$calcInteract) {
            map$data <- data.frame(map$data, map$interact)
        }
        if(map$calcPoly) {
            map$data <- data.frame(map$data,
                                   do.call("data.frame", map$poly)
                                   )
        }

        ## Note the variables we need to scale down below:
        scaleNames <- c(colnames(map$interact),
                        unlist(
                            lapply(map$poly, colnames)
                        )
                        )
        colnames(map$data) <- c(map$idVars, scaleNames)

        if(!map$simMode) {
            ## Make sure the number of PC scores we want is less than
            ## the number of columns in our data object:
            datCols <- ncol(map$data) - length(map$idVars)
            if( nComps > datCols ) {
                warnFun("nonLinPcNum", map)
                nComps <- map$nComps[2] <- datCols
            }
        }

        ## Remove the contents of the 'interact' and 'poly'
        ## fields when they are no longer necessary:
        map$interact <- "Removed to save resources"
        map$poly <- "Removed to save resources"
    }# CLOSE if( is.null(map$pcAux$lin) )

    ## Execute the principal component analysis:
    if(map$pcaMemLev == 0) {
        ## Higher numerical accuracy, but more memory usage
        pcaOut <- prcomp(map$data[ , setdiff(colnames(map$data), map$idVars)],
                         scale = TRUE,
                         retx = TRUE)
    } else if(map$pcaMemLev == 1) {
        ## Save memory at the expense of numerical accuracy
        map$data[ , scaleNames] <- lowMemScale(map$data[ , scaleNames])

        pcaOut <-
            simplePca(inData = map$data[ ,
                          setdiff(colnames(map$data), map$idVars)],
                      nComps = nComps)
    } else {
        errFun("badPcaMemLev", map = map)
    }

    ## Extract the principal component scores:
    map$pcAux[[linVal]] <-
        data.frame(map$data[ , map$idVars], pcaOut$x[ , 1 : nComps])

    ## Remove the contents of the 'data' field
    ## when they're no longer needed:
    if(linVal == "nonLin") map$data <- "Removed to save resources"

    ## Give some informative column names:
    if(linVal == "lin") {
        colnames(map$pcAux[[linVal]]) <- c(map$idVars,
                                           paste0("linPC", c(1 : nComps) )
                                           )
    } else {
        colnames(map$pcAux[[linVal]]) <- c(map$idVars,
                                           paste0("nonLinPC", c(1 : nComps) )
                                           )
    }

    ## Save the components' variances:
    map$rSquared[[linVal]] <- pcaOut$sdev

    rm(pcaOut) # clear some memory
    rm(scaleNames)

    ## Compute the cumulative variance explained:
    totalVar <- sum(map$rSquared[[linVal]], na.rm = TRUE)

    map$rSquared[[linVal]][1] <-
        map$rSquared[[linVal]][1] / totalVar

    for( i in 2 : length(map$rSquared[[linVal]]) ) {
        map$rSquared[[linVal]][i] <-
            map$rSquared[[linVal]][i - 1] +
                (map$rSquared[[linVal]][i] / totalVar)
    }

    rm(totalVar)
    silentGC()
    if(map$verbose) cat("Complete.\n")
}# END doPCA()




## Create nonlinear terms and orthogonalize them:
computeNonlinearTerms <- function(map)
{
    if(map$verbose) cat("\nComputing interaction and polynomial terms...\n")

    ## Get variable names without IDs included:
    dataNames <- setdiff(colnames(map$data), map$idVars)
    pcNames <- setdiff(colnames(map$pcAux$lin), map$idVars)

    if(map$calcInteract) {
        ## Construct all possible two-way interactions between the comps
        ## and the raw variables and orthogonalize them:
        intCombMat <- expand.grid(dataNames,
                                  pcNames,
                                  stringsAsFactors = FALSE)
        map$interact <- data.frame(
            matrix(NA, nrow(map$data), nrow(intCombMat))
        )

        ## Orthogonalize the interaction terms w.r.t. their
        ## constituent raw variables and PC scores:
        for( i in 1 : nrow(intCombMat) ) {
            ## Compute the interaction term:
            tmp1 <- map$data[ , intCombMat[i, 1] ] *
                map$pcAux$lin[ , intCombMat[i, 2] ]

            ## Orthogonalize the interaction term and save it:
            map$interact[ , i] <-
                .lm.fit(y = tmp1,
                        x = as.matrix(map$pcAux$lin[ , pcNames])
                        )$resid
        }

        colnames(map$interact) <-
            paste0(intCombMat[ , 1], "_", intCombMat[ , 2])

        rm(intCombMat)
        rm(tmp1)
    } else {
        map$interact <- NULL
    }# CLOSE if(calcInteract)

    ## Compute powered terms and orthogonalize them:
    if(map$calcPoly) {

        dataNames <- setdiff(dataNames, map$dummyVars)

        for(p in 1 : (map$maxPower - 1)) {# Loop over power levels
            powerVals <- c("square", "cube", "quad")

            ## Compute the powered terms and orthogonalize them
            ## w.r.t. their lower-powered counterparts and the linear pcAux:
            map$poly[[ powerVals[p] ]] <-
                apply(map$data[ , dataNames],
                      2,
                      FUN = function(dv, pp, pcAux) {
                          .lm.fit(y = dv^pp,
                                  x = as.matrix(
                                      cbind(
                                          sapply(c((pp - 1) : 1),
                                                 FUN = function(ppp, dat) {
                                                     dat^ppp
                                                 },
                                                 dat = dv),
                                          pcAux)
                                  )
                                  )$resid
                      },
                      pp = p + 1,
                      pcAux = map$pcAux$lin[ , pcNames]
                      )

            ## Give some sensible variable names:
            colnames(map$poly[[ powerVals[p] ]]) <-
                paste0(colnames(map$data[ , dataNames]),
                       "_p",
                       p + 1)

        }# END for(p in 1 : (map$maxPower - 1))

        rm(powerVals)
    } else {
        map$poly <- NULL
    }# CLOSE if(calcPoly)

    silentGC()
    if(map$verbose) cat("Complete.\n")
}# END computeNonlinearTerms()



## Construct one imputated data set with mice
## for use in parallel processing
parallelMice <- function(imp, predMat, map)
{
    ## Setup the PRNG:
    .lec.SetPackageSeed(rep(map$seed, 6))
    if( !imp %in% .lec.GetStreams() )
        .lec.CreateStream(c(1 : map$nImps))
    .lec.CurrentStream(imp)

    ## Create a single imputation:
    miceOut <- try(
        mice(map$data,
             m = 1L,
             maxit = 1L,
             predictorMatrix = predMat,
             method = map$methVec,
             printFlag = map$verbose,
             ridge = map$miceRidge),
        silent = FALSE)

    if(class(miceOut) != "try-error") {
        impData <- complete(miceOut, 1)
    } else {
        impData <- NULL
        warnFun("miceCrash",
                map = list(impNum = imp, miceObj = miceOut)
                )
    }
    impData
}
