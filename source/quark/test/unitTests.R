### Title:    Unit Tests for Quark
### Author:   Kyle M. Lang
### Created:  2015-NOV-01
### Modified: 2017-MAR-09

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

rm(list = ls(all = TRUE))

library(quark)
library(mvtnorm)
source("makeTestData.R")


##### DATA PREP TESTING #####

## Test:   Normal usage with one ID
## Result: Successful execution
quarkData <- prepData(rawData   = testData,
                      nomVars   = c("nom1", "nom2"),
                      ordVars   = c("ord1", "ord2"),
                      idVars    = c("id1"),
                      dropVars  = c("y1", "y2", "z2", "id2"),
                      groupVars = c("nom1", "nom2", "ord1")
                      )

## Test:   Catch high percent missing
## Result: Warning with request for user input
quarkData <- prepData(rawData   = testData,
                      nomVars   = c("nom1", "nom2"),
                      ordVars   = c("ord1", "ord2"),
                      idVars    = c("id1", "id2"),
                      dropVars  = c("y1", "y2"),
                      groupVars = c("nom1", "nom2", "ord1")
                      )

frozenQuarkData1 <- quarkData$copy()

## Test:   Find bad nominals
## Result: Warning with request for user input
quarkData <- prepData(rawData   = testData,
                      nomVars   = c("nom1", "nom2", "bigCat"),
                      ordVars   = c("ord1", "ord2"),
                      idVars    = c("id1", "id2"),
                      dropVars  = c("y1", "y2", "z2"),
                      groupVars = c("nom1", "nom2", "ord1")
                      )

## Test:   Find bad ordinals
## Result: Warning with request for user input
quarkData <- prepData(rawData   = testData,
                      nomVars   = c("nom1", "nom2"),
                      ordVars   = c("ord1", "ord2", "bigCat"),
                      idVars    = c("id1", "id2"),
                      dropVars  = c("y1", "y2", "z2"),
                      groupVars = c("nom1", "nom2", "ord1")
                      )

## Test:   Find bad continuous
## Result: Warning with request for user input
quarkData <- prepData(rawData   = testData,
                      nomVars   = c("nom1", "nom2"),
                      ordVars   = c("ord1"),
                      idVars    = c("id1", "id2"),
                      dropVars  = c("y1", "y2", "z2"),
                      groupVars = c("nom1", "nom2", "ord1")
                      )

## Replace IDs with fully observed versions to test 'simMode'
testData2 <- testData
testData2[ , c("id1", "id2")] <-
    matrix(seq(1 : (2 * nrow(testData2))), ncol = 2)

## Test:   Use simMode = TRUE
## Result: Successful execution with nothing printed to stdout
quarkData <- prepData(rawData   = testData2,
                      nomVars   = c("nom1", "nom2"),
                      ordVars   = c("ord1", "ord2"),
                      idVars    = c("id1", "id2"),
                      dropVars  = c("x3", "z1", "y5"),
                      groupVars = c("nom1", "nom2", "ord1"),
                      simMode   = TRUE)

frozenQuarkData2 <- quarkData$copy()

## Test:   Trip initial check for non-existant variables
## Result: Fatal Error
quarkData <- prepData(rawData   = testData,
                      nomVars   = c("nom1", "nom2", "bob"),
                      ordVars   = c("ord1", "ord2"),
                      idVars    = c("id1", "id2", "suzy"),
                      dropVars  = c("y1", "y2"),
                      groupVars = c("nom1", "nom2", "ord1")
                      )

## Test:   Trip initial check for non-distinct dropVars
## Result: Fatal Error
quarkData <- prepData(rawData   = testData,
                      nomVars   = c("nom1", "nom2"),
                      ordVars   = c("ord1", "ord2"),
                      idVars    = c("id1", "id2"),
                      dropVars  = c("y1", "y2", "nom1"),
                      groupVars = c("nom1", "nom2", "ord1")
                      )

## Test:   Trip initial check for non-distinct idVars
## Result: Fatal Error
quarkData <- prepData(rawData   = testData,
                      nomVars   = c("nom1", "nom2", "id2"),
                      ordVars   = c("ord1", "ord2"),
                      idVars    = c("id1", "id2"),
                      dropVars  = c("y1", "y2"),
                      groupVars = c("nom1", "nom2", "ord1")
                      )

## Test:   Use parallel processing to check for collinearity
## Result: Successful execution
quarkData <- prepData(rawData     = testData,
                      nomVars     = c("nom1", "nom2"),
                      ordVars     = c("ord1", "ord2"),
                      idVars      = c("id1", "id2"),
                      dropVars    = c("y1", "y2", "z2"),
                      groupVars   = c("nom1", "nom2", "ord1"),
                      useParallel = TRUE,
                      nProcess    = 4L)

## Test:   Specify key moderators variables
## Result: Successful execution
quarkData <- prepData(rawData    = testData,
                      moderators = c("x1", "nom1", "ord1"),
                      nomVars    = c("nom1", "nom2"),
                      ordVars    = c("ord1", "ord2"),
                      idVars     = c("id1", "id2"),
                      dropVars   = c("y1", "y2", "z2"),
                      groupVars  = c("nom1", "nom2", "ord1")
                      )
frozenQuarkData3 <- quarkData$copy()

## Test:   Make sure collinear moderators are not dropped
## Result: Successful execution
quarkData <- prepData(rawData    = testData,
                      moderators = "y4",
                      nomVars    = c("nom1", "nom2"),
                      ordVars    = c("ord1", "ord2"),
                      idVars     = c("id1"),
                      dropVars   = c("y1", "y2", "z2", "id2"),
                      groupVars  = c("nom1", "nom2", "ord1")
                      )

## Test:   Make sure moderators' collinear partner is dropped
## Result: Successful execution
quarkData <- prepData(rawData    = testData,
                      moderators = "x2",
                      nomVars    = c("nom1", "nom2"),
                      ordVars    = c("ord1", "ord2"),
                      idVars     = c("id1"),
                      dropVars   = c("y1", "y2", "z2", "id2"),
                      groupVars  = c("nom1", "nom2", "ord1")
                      )

## Test:   Make sure both collinear moderators are retained
## Result: Successful execution
quarkData <- prepData(rawData    = testData,
                      moderators = c("y4", "x2"),
                      nomVars    = c("nom1", "nom2"),
                      ordVars    = c("ord1", "ord2"),
                      idVars     = c("id1"),
                      dropVars   = c("y1", "y2", "z2", "id2"),
                      groupVars  = c("nom1", "nom2", "ord1")
                      )


##### PCAUX CREATION TESTING #####


## Test:   Basic usage
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData = tmp)

frozenPcAuxOut1 <- pcAuxOut$copy()

## Test:   Use forcePmm = TRUE
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData = tmp,
                        forcePmm  = TRUE)

## Test:   Use low memory pca
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData = tmp,
                        control   = list(pcaMemLevel = 1L)
                        )

## Test:   Use simMode = TRUE and forcePmm = TRUE
## Result: Successful execution with nothing printed to stdout
tmp      <- frozenQuarkData2$copy()
pcAuxOut <- createPcAux(quarkData = tmp,
                        simMode   = TRUE,
                        forcePmm  = TRUE)

frozenPcAuxOut2 <- pcAuxOut$copy()

## Test:   Extract 0 non-linear components
## Result: Successful exectution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData = tmp,
                        nComps    = c(10, 0)
                        )

frozenPcAuxOut3 <- pcAuxOut$copy()

## Test:   Try extracting 0 linear components
## Result: Fatal Error
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData = tmp,
                        nComps    = c(0, 3)
                        )

## Test:   Don't use interactions
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 0)

## Test:   Don't use polynomials
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData  = tmp,
                        maxPolyPow = 1)

## Test:   Only use squares
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData  = tmp,
                        maxPolyPow = 2L)

## Test:   Use polynomials up to 4th power
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData  = tmp,
                        maxPolyPow = 4L)

## Test:   Try to request polynomial power greater than 4
## Result: Fatal Error
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData  = tmp,
                        maxPolyPow = 5L)

## Test:   Try to request nComps[2] > 0 while asking for no nonlinearities
## Result: Fatal Error
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 0,
                        maxPolyPow   = 1)

## Test:   Don't specify any ID variables
## Result: Successful Execution
quarkData <- prepData(rawData  = testData,
                      nomVars  = c("nom1", "nom2"),
                      ordVars  = c("ord1", "ord2"),
                      dropVars = c("y1", "y2", "z2", "id1", "id2")
                      )
pcAuxOut <- createPcAux(quarkData = quarkData)

## Test:   Don't specify any dropped variables
## Result: Successful execution
quarkData <- prepData(rawData = testData,
                      nomVars = c("nom1", "nom2"),
                      ordVars = c("ord1", "ord2"),
                      idVars  = c("id1", "id2")
                      )
pcAuxOut <- createPcAux(quarkData = quarkData)

frozenPcAuxOut4 <- pcAuxOut$copy()

## Simulate some clean data:
sigma       <- matrix(0.3, 50, 50)
diag(sigma) <- 1.0

testData3           <- rmvnorm(500, rep(0, 50), sigma)
colnames(testData3) <- paste0("x", c(1 : 50))

testData3[as.logical(rbinom(prod(dim(testData3)), 1, 0.2))] <- NA

## Test:   Run quark on clean, normally distributed data
## Result: Successful execution
quarkData <- prepData(rawData = testData3)
pcAuxOut  <- createPcAux(quarkData = quarkData)

frozenPcAuxOut6 <- pcAuxOut$copy()

## Test:   Run quark on clean, normally distributed data using simMode = TRUE
## Result: Successful execution
quarkData <- prepData(rawData = as.data.frame(testData3),
                      simMode = TRUE)
pcAuxOut <- createPcAux(quarkData = quarkData,
                        simMode   = TRUE)

frozenPcAuxOut7 <- pcAuxOut$copy()

## Simulate some P > N data:
sigma       <- matrix(0.3, 100, 100)
diag(sigma) <- 1.0

testData4           <- rmvnorm(75, rep(0, 100), sigma)
colnames(testData4) <- paste0("x", c(1 : 100))

testData4[as.logical(rbinom(prod(dim(testData4)), 1, 0.2))] <- NA

quarkData <- prepData(rawData = as.data.frame(testData4))
pcAuxOut  <- createPcAux(quarkData = quarkData)

frozenPcAuxOut8 <- pcAuxOut$copy()

## Test:   Specify interactions among observed variables
## Result: Successful execution
tmp      <- frozenQuarkData3$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 1)

## Test:   Specify interactions among observed variables without polynomials
## Result: Successful execution
tmp      <- frozenQuarkData3$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        nComps       = c(5, 0),
                        interactType = 1)

## Test:   Specify interactions between key moderators and PcAux
## Result: Successful execution
tmp      <- frozenQuarkData3$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 2)

## Test:   Specify moderators but don't use them
## Result: Successful execution
tmp      <- frozenQuarkData3$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3)

## Test:   Specify number of PcAux in terms of variance explained
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3,
                        nComps       = c(0.5, 0.5)
                        )

## Test:   Specify number of PcAux in terms of variance explained and include
##         interactions between PcAux and key moderators
## Result: Successful execution
tmp      <- frozenQuarkData3$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 2,
                        nComps       = c(0.5, 0.5)
                        )

## Test:   Specify number of PcAux in mixed terms
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3,
                        nComps       = c(5, 0.5)
                        )

## Test:   Specify number of PcAux in mixed terms 2
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3,
                        nComps       = c(0.5, 5)
                        )

## Test:   Specify number of PcAux with special keyword
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3,
                        nComps       = c(Inf, Inf)
                        )
frozenPcAuxOut9 <- pcAuxOut$copy()

## Test:   Specify number of PcAux in mixed terms 3
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3,
                        nComps       = c(Inf, 5)
                        )

## Test:   Specify number of PcAux in mixed terms 4
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3,
                        nComps       = c(0.75, Inf)
                        )

## Test:   Get all possible PcAux
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3,
                        nComps       = c(-100, -100)
                        )
frozenPcAuxOut10 <- pcAuxOut$copy()

## Test:   Specify number of PcAux in mixed terms 5
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3,
                        nComps       = c(Inf, -1)
                        )

## Test:   Specify number of PcAux in mixed terms 6
## Result: Successful execution
tmp      <- frozenQuarkData1$copy()
pcAuxOut <- createPcAux(quarkData    = tmp,
                        interactType = 3,
                        nComps       = c(4, -11)
                        )


##### MI TESTING #####


## Test:   Ordinary usage
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     nImps     = 5L)

## Test:   Use forcePmm = TRUE
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = "z2",
                     nImps     = 5L,
                     forcePmm  = TRUE)

## Test:   Use simMode = TRUE
## Result: Successful execution with nothing printed to stdout
tmp   <- frozenPcAuxOut2$copy()
miOut <- miWithPcAux(rawData   = testData2,
                     quarkData = tmp,
                     dropVars  = c("z2", "z1", "y5"),
                     nImps     = 5L,
                     simMode   = TRUE)

## Test:   Use simMode = TRUE and forcePmm = TRUE
## Result: Successful execution with nothing printed to stdout
tmp   <- frozenPcAuxOut2$copy()
miOut <- miWithPcAux(rawData   = testData2,
                     quarkData = tmp,
                     dropVars  = c("z2", "z1", "y5"),
                     nImps     = 5L,
                     simMode   = TRUE,
                     forcePmm  = TRUE)

## Test:   Defining nComps in terms of variance explained
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = "z2",
                     nImps     = 5L,
                     nComps    = c(0.5, 0.05)
                     )

## Test:   Defining nComps in terms of 'Inf' keyword
## Result: Successful execution
tmp   <- frozenPcAuxOut9$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = c("z2", "ord2"),
                     nImps     = 2L,
                     nComps    = c(Inf, Inf)
                     )

## Test:   Using all possible PcAux:
## Result: Successful execution
tmp   <- frozenPcAuxOut10$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = c("z2", "ord2"),
                     nImps     = 2L,
                     nComps    = c(-1, -1)
                     )

## Test:   Asking for too much linear variance explained
## Result: Fatal Error
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  ="z2",
                     nImps     = 5L,
                     nComps    = c(0.9, 0.05)
                     )

## Test:   Asking for too much non-linear variance explained
## Result: Fatal Error
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = "z2",
                     nImps     = 5L,
                     nComps    = c(0.5, 0.5)
                     )

## Test:   Asking for too many linear components
## Result: Fatal Error
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = "z2",
                     nImps     = 5L,
                     nComps    = c(42, 3)
                     )

## Test:   Asking for too many non-linear components
## Result: Fatal Error
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = "z2",
                     nImps     = 5L,
                     nComps    = c(10, 42)
                     )

## Test:   Use defaults with no non-linear pcAux:
## Result: Successful execution
tmp   <- frozenPcAuxOut3$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     nImps     = 5L)

## Test:   Request no non-linear components
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = "z2",
                     nImps     = 5L,
                     nComps    = c(5, 0)
                     )

## Test:   Try requesting no linear components
## Result: Fatal Error denoting the requirement for nLinear >= 1
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = "z2",
                     nImps     = 5L,
                     nComps    = c(0, 5)
                     )

## Test:   Specify nomVars different from quarkObj$nomVars
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     nomVars   = "nom1",
                     dropVars  = c("z2", "nom2"),
                     nImps     = 5L)

## Test:   Specify ordVars different from quarkObj$ordVars
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     ordVars   = "ord1",
                     dropVars  = c("z2", "ord2"),
                     nImps     = 5L)

## Test:   Specify idVars different from quarkObj$idVars
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     nomVars   = "nom2",
                     idVars    = c("id1", "id2", "nom1"),
                     dropVars  = "z2",
                     nImps     = 5L)

## Test:   Trip initial check for distinct idVars
## Result: Fatal Error
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     idVars    = c("id1", "id2", "nom1"),
                     dropVars  = "z2",
                     nImps     = 5L)

## Test:   Trip initial check for distinct dropVars
## Result: Fatal Error
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     idVars    = c("id1", "id2"),
                     dropVars  = c("id2", "z2")
                     )

## Test:   Trip initial check for non-existant variables
## Result: Fatal Error
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     idVars    = c("id1", "id2", "suzy"),
                     dropVars  = c("z2", "z1", "y5")
                     )

## Test:   Request 'long' format for imputed data sets
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData        = testData,
                     quarkData      = tmp,
                     dropVars       = "z2",
                     nImps          = 5L,
                     completeFormat = "long")

## Test:   Request 'broad' format for imputed data sets
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData        = testData,
                     quarkData      = tmp,
                     dropVars       = "z2",
                     nImps          = 5L,
                     completeFormat = "broad")

## Test:   Request 'repeated' format for imputed data sets
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData        = testData,
                     quarkData      = tmp,
                     dropVars       = "z2",
                     nImps          = 5L,
                     completeFormat = "repeated")

## Test:   Call miWithPcAux() with no user-defined dropVars in prepData():
## Result: Successful execution
tmp   <- frozenPcAuxOut4$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     nImps     = 5L)

## Test:   Use miWithPcAux() with clean, normally distributed data:
## Result: Successful execution
tmp   <- frozenPcAuxOut6$copy()
miOut <- miWithPcAux(rawData   = testData3,
                     quarkData = tmp,
                     nImps     = 5L)

## Test:   Use miWithPcAux() with clean, normally distributed data with
##         simMode = TRUE
## Result: Successful execution
tmp   <- frozenPcAuxOut7$copy()
miOut <- miWithPcAux(rawData   = testData3,
                     quarkData = tmp,
                     nImps     = 5L,
                     simMode   = TRUE)

## Test:   Use parallel processing to generate the multiple imputations
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData   = testData,
                     quarkData = tmp,
                     dropVars  = "z2",
                     nImps     = 5L,
                     nProcess  = 4)

## Test:   Use parallel processing with "long" output
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData        = testData,
                     quarkData      = tmp,
                     dropVars       = "z2",
                     nImps          = 5L,
                     nProcess       = 4,
                     completeFormat = "long")

## Test:   Use parallel processing with "broad" output
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData        = testData,
                     quarkData      = tmp,
                     dropVars       = "z2",
                     nImps          = 5L,
                     nProcess       = 4,
                     completeFormat = "broad")

## Test:   Use parallel processing with "repeated" output
## Result: Successful execution
tmp   <- frozenPcAuxOut1$copy()
miOut <- miWithPcAux(rawData        = testData,
                     quarkData      = tmp,
                     dropVars       = "z2",
                     nImps          = 5L,
                     nProcess       = 4,
                     completeFormat = "repeated")
