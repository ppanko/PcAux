### Title:    Unit Tests for Quark
### Author:   Kyle M. Lang
### Created:  2015-NOV-01
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

rm(list = ls(all = TRUE))

library(quark)
library(mvtnorm)
source("makeTestData.R")


##### DATA PREP TESTING #####

## Test:   Catch high percent missing
## Result: Warning with request for user input
quarkData1 <- prepData(rawData = testData,
                       nomVars = c("nom1", "nom2"),
                       ordVars = c("ord1", "ord2"),
                       idVars = c("id1", "id2"),
                       dropVars = c("y1", "y2"),
                       groupVars = c("nom1", "nom2", "ord1")
                       )

frozenQuarkData1 <- quarkData1$copy()

## Test:   Find bad nominals
## Result: Warning with request for user input
quarkData2 <- prepData(rawData = testData,
                       nomVars = c("nom1", "nom2", "bigCat"),
                       ordVars = c("ord1", "ord2"),
                       idVars = c("id1", "id2"),
                       dropVars = c("y1", "y2", "z2"),
                       groupVars = c("nom1", "nom2", "ord1")
                       )

## Test:   Find bad ordinals
## Result: Warning with request for user input
quarkData3 <- prepData(rawData = testData,
                       nomVars = c("nom1", "nom2"),
                       ordVars = c("ord1", "ord2", "bigCat"),
                       idVars = c("id1", "id2"),
                       dropVars = c("y1", "y2", "z2"),
                       groupVars = c("nom1", "nom2", "ord1")
                       )

## Test:   Find bad continuous
## Result: Warning with request for user input
quarkData4 <- prepData(rawData = testData,
                       nomVars = c("nom1", "nom2"),
                       ordVars = c("ord1"),
                       idVars = c("id1", "id2"),
                       dropVars = c("y1", "y2", "z2"),
                       groupVars = c("nom1", "nom2", "ord1")
                       )

## Replace IDs with fully observed versions to test 'simMode'
testData2 <- testData
testData2[ , c("id1", "id2")] <-
    matrix(seq(1 : (2 * nrow(testData2))), ncol = 2)

## Test:   Use simMode = TRUE
## Result: Successful execution with nothing printed to stdout
quarkData5 <- prepData(rawData = testData2,
                       nomVars = c("nom1", "nom2"),
                       ordVars = c("ord1", "ord2"),
                       idVars = c("id1", "id2"),
                       dropVars = c("x3", "z1", "y5"),
                       groupVars = c("nom1", "nom2", "ord1"),
                       simMode = TRUE)

frozenQuarkData5 <- quarkData5$copy()

## Test:   Trip initial check for non-existant variables
## Result: Fatal Error
quarkData6 <- prepData(rawData = testData,
                       nomVars = c("nom1", "nom2", "bob"),
                       ordVars = c("ord1", "ord2"),
                       idVars = c("id1", "id2", "suzy"),
                       dropVars = c("y1", "y2"),
                       groupVars = c("nom1", "nom2", "ord1")
                       )

## Test:   Trip initial check for non-distinct dropVars
## Result: Fatal Error
quarkData7 <- prepData(rawData = testData,
                       nomVars = c("nom1", "nom2"),
                       ordVars = c("ord1", "ord2"),
                       idVars = c("id1", "id2"),
                       dropVars = c("y1", "y2", "nom1"),
                       groupVars = c("nom1", "nom2", "ord1")
                       )

## Test:    Trip initial check for non-distinct idVars
## Results: Fatal Error
quarkData8 <- prepData(rawData = testData,
                       nomVars = c("nom1", "nom2", "id2"),
                       ordVars = c("ord1", "ord2"),
                       idVars = c("id1", "id2"),
                       dropVars = c("y1", "y2"),
                       groupVars = c("nom1", "nom2", "ord1")
                       )


##### QUARK TESTING #####


## Test:   Basic usage
## Result: Successful execution
tmp <- frozenQuarkData1$copy()
pcAuxOut1 <- createPcAux(quarkData = tmp)

frozenPcAuxOut1 <- pcAuxOut1$copy()

## Test:   Use forcePmm = TRUE
## Result: Successful execution
tmp <- frozenQuarkData1$copy()
pcAuxOut2 <- createPcAux(quarkData = tmp,
                         forcePmm = TRUE)

## Test:   Use low memory pca
## Result: Successful execution
tmp <- frozenQuarkData1$copy()
pcAuxOut3 <- createPcAux(quarkData = tmp,
                         pcaMemLevel = 1L)

## Test:   Use simMode = TRUE and forcePmm = TRUE
## Result: Successful execution with nothing printed to stdout
tmp <- frozenQuarkData5$copy()
pcAuxOut4 <- createPcAux(quarkData = tmp,
                         simMode = TRUE,
                         forcePmm = TRUE)

frozenPcAuxOut4 <- pcAuxOut4$copy()

## Test:   Invoke fall-back PMM
## Result: Induced failure of first-pass imputation
##         with successful execution of fall-back PMM
tmp <- frozenQuarkData1$copy()
pcAuxOut5 <- createPcAux(quarkData = tmp,
                         skipFirstPass = TRUE)

## Test:   Invoke fall-back group-mean substitution
## Result: Induced failure of first-pass imputation and PMM
##         with successful execution of fall-back group-mean substition
tmp <- frozenQuarkData1$copy()
pcAuxOut6 <- createPcAux(quarkData = tmp,
                         skipPmm = TRUE)

## Test:   Invoke group-mean substitution with one continuous grouping variable
## Result: Induced failure of first-pass imputation and PMM
##         with successful execution of fall-back group-mean substitution
quarkData9 <- prepData(rawData = testData,
                       nomVars = c("nom1", "nom2"),
                       ordVars = c("ord1", "ord2"),
                       idVars = c("id1", "id2"),
                       dropVars = c("y1", "y2", "z2"),
                       groupVars = c("nom1", "ord1", "w1")
                       )

pcAuxOut7 <- createPcAux(quarkData = quarkData9,
                         skipPmm = TRUE)

## Test:   Invoke group-mean substitution with multiple continuous grouping variables
## Result: Induced failure of first-pass imputation and PMM
##         with successful execution of fall-back group-mean substitution
quarkData10 <- prepData(rawData = testData,
                        nomVars = c("nom1", "nom2"),
                        ordVars = c("ord1", "ord2"),
                        idVars = c("id1", "id2"),
                        dropVars = c("y1", "y2", "z2"),
                        groupVars = c("nom1", "ord1", "w1", "w2")
                        )

pcAuxOut8 <- createPcAux(quarkData = quarkData10,
                         skipPmm = TRUE)

## Test:   Invoke group-mean substitution with no grouping variables defined
## Result: Print a warning and fall-back to grand-mean substitution
quarkData11 <- prepData(rawData = testData,
                        nomVars = c("nom1", "nom2"),
                        ordVars = c("ord1", "ord2"),
                        idVars = c("id1", "id2"),
                        dropVars = c("y1", "y2", "z2"),
                        )

pcAuxOut9 <- createPcAux(quarkData = quarkData11,
                         skipPmm = TRUE)

## Test:   Invoke fall-back grand-mean substitution
## Result: Induced failure of first-pass imputation, PMM, and group-mean
##         substitution with successful execution of grand-mean substitution
tmp <- frozenQuarkData1$copy()
pcAuxOut10 <- createPcAux(quarkData = tmp,
                          skipGroupMean = TRUE)

## Test:   Invoke imputation failure state
## Result: Induced failure of first-pass imputation, PMM, group-mean
##         substitution, and grand-mean substitution.
tmp <- frozenQuarkData1$copy()
pcAuxOut11 <- createPcAux(quarkData = tmp,
                          skipGrandMean = TRUE)

## Test:    Extract 0 non-linear components
## Results: Successful exectution
tmp <- frozenQuarkData1$copy()
pcAuxOut12 <- createPcAux(quarkData = tmp,
                          nComps = c(10, 0)
                          )

frozenPcAuxOut12 <- pcAuxOut12$copy()

## Test:   Try extracting 0 linear components
## Result: Fatal Error
tmp <- frozenQuarkData1$copy()
pcAuxOut13 <- createPcAux(quarkData = tmp,
                          nComps = c(0, 3)
                          )

## Test:   Don't use interactions
## Result: Successful execution
tmp <- frozenQuarkData1$copy()
pcAuxOut14 <- createPcAux(quarkData = tmp,
                          useInteract = FALSE
                          )

## Test:   Don't use polynomials
## Result: Successful execution
tmp <- frozenQuarkData1$copy()
pcAuxOut15 <- createPcAux(quarkData = tmp,
                          usePoly = FALSE
                          )

## Test:   Only use squares
## Result: Successful execution
tmp <- frozenQuarkData1$copy()
pcAuxOut16 <- createPcAux(quarkData = tmp,
                          maxPower = 2L
                          )

## Test:   Use polynomials up to 4th power
## Result: Successful execution
tmp <- frozenQuarkData1$copy()
pcAuxOut17 <- createPcAux(quarkData = tmp,
                          maxPower = 4L
                          )

## Test:   Try to request only first-order polynomials
## Result: Fatal Error
tmp <- frozenQuarkData1$copy()
pcAuxOut18 <- createPcAux(quarkData = tmp,
                          maxPower = 1L
                          )

## Test:   Try to request polynomial power greater than 4
## Result: Fatal Error
tmp <- frozenQuarkData1$copy()
pcAuxOut19 <- createPcAux(quarkData = tmp,
                          maxPower = 5L
                          )

## Test:   Try to request nComps[2] > 0 while asking for no nonlinearities
## Result: Fatal Error
tmp <- frozenQuarkData1$copy()
pcAuxOut20 <- createPcAux(quarkData = tmp,
                          useInteract = FALSE,
                          usePoly = FALSE)

## Test:   Don't specify any ID variables
## Result: Successful Execution
quarkData12 <- prepData(rawData = testData,
                        nomVars = c("nom1", "nom2"),
                        ordVars = c("ord1", "ord2"),
                        dropVars = c("y1", "y2", "z2")
                        )
pcAuxOut21 <- createPcAux(quarkData = quarkData12)

## Test:   Don't specify any dropped variables
## Result: Successful execution
quarkData13 <- prepData(rawData = testData,
                        nomVars = c("nom1", "nom2"),
                        ordVars = c("ord1", "ord2"),
                        idVars = c("id1", "id2")
                        )
pcAuxOut22 <- createPcAux(quarkData = quarkData13)

frozenPcAuxOut22 <- pcAuxOut22$copy()

## Test:   Try to use a grouping variable that was dropped via data checks:
## Result: Successful execution
quarkData14 <- prepData(rawData = testData,
                        nomVars = c("nom1", "nom2"),
                        ordVars = c("ord1", "ord2"),
                        idVars = c("id1", "id2"),
                        dropVars = c("y1", "y2", "z2"),
                        groupVars = c("nom1", "nom2", "y4")
                        )
pcAuxOut23 <- createPcAux(quarkData = quarkData14,
                          skipPmm = TRUE)

frozenPcAuxOut23 <- pcAuxOut23$copy()

## Simulate some clean data:
sigma <- matrix(0.3, 50, 50)
diag(sigma) <- 1.0
testData3 <- rmvnorm(500, rep(0, 50), sigma)
colnames(testData3) <- paste0("x", c(1 : 50))
testData3[as.logical(rbinom(prod(dim(testData3)), 1, 0.2))] <- NA

## Test:   Run quark on clean, normally distributed data
## Result: Successful execution
quarkData15 <- prepData(rawData = testData3)
pcAuxOut24 <- createPcAux(quarkData = quarkData15)

frozenPcAuxOut24 <- pcAuxOut24$copy()

## Test:   Run quark on clean, normally distributed data using simMode = TRUE
## Result: Successful execution
quarkData16 <- prepData(rawData = as.data.frame(testData3),
                        simMode = TRUE)
pcAuxOut25 <- createPcAux(quarkData = quarkData16,
                          simMode = TRUE)

frozenPcAuxOut25 <- pcAuxOut25$copy()

## Simulate some P > N data:
sigma <- matrix(0.3, 100, 100)
diag(sigma) <- 1.0
testData4 <- rmvnorm(75, rep(0, 100), sigma)
colnames(testData4) <- paste0("x", c(1 : 100))
testData4[as.logical(rbinom(prod(dim(testData4)), 1, 0.2))] <- NA

quarkData17 <- prepData(rawData = as.data.frame(testData4))
pcAuxOut26 <- createPcAux(quarkData = quarkData17)

frozenPcAuxOut26 <- pcAuxOut26$copy()


##### ROM TESTING #####


## Test:   Ordinary usage
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut1 <- miWithPcAux(rawData = testData,
                      quarkData = tmp,
                      nImps = 5L)

## Test:    Use forcePmm = TRUE
## Results: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut2 <- miWithPcAux(rawData = testData,
                      quarkData = tmp,
                      dropVars = "z2",
                      nImps = 5L,
                      forcePmm = TRUE)

## Test:   Use simMode = TRUE
## Result: Successful execution with nothing printed to stdout
tmp <- frozenPcAuxOut4$copy()
miOut3 <- miWithPcAux(rawData = testData2,
                      quarkData = tmp,
                      dropVars = c("z2", "z1", "y5"),
                      nImps = 5L,
                      simMode = TRUE)

## Test:   Use simMode = TRUE and forcePmm = TRUE
## Result: Successful execution with nothing printed to stdout
tmp <- frozenPcAuxOut4$copy()
miOut4 <- miWithPcAux(rawData = testData2,
                      quarkData = tmp,
                      dropVars = c("z2", "z1", "y5"),
                      nImps = 5L,
                      simMode = TRUE,
                      forcePmm = TRUE)

## Test:   Defining nComps in terms of variance explained
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut5 <- miWithPcAux(rawData = testData,
                      quarkData = tmp,
                      dropVars = "z2",
                      nImps = 5L,
                      varExpLin = 0.5,
                      varExpNonLin = 0.05)

## Test:   Asking for too much linear variance explained
## Result: Fatal Error
tmp <- frozenPcAuxOut1$copy()
miOut6 <- miWithPcAux(rawData = testData,
                      quarkData = tmp,
                      dropVars ="z2",
                      nImps = 5L,
                      varExpLin = 0.9)

## Test:   Asking for too much non-linear variance explained
## Result: Fatal Error
tmp <- frozenPcAuxOut1$copy()
miOut7 <- miWithPcAux(rawData = testData,
                      quarkData = tmp,
                      dropVars = "z2",
                      nImps = 5L,
                      varExpNonLin = 0.25)

## Test:   Asking for too many linear components
## Result: Fatal Error
tmp <- frozenPcAuxOut1$copy()
miOut8 <- miWithPcAux(rawData = testData,
                      quarkData = tmp,
                      dropVars = "z2",
                      nImps = 5L,
                      nLinear = 42)

## Test:   Asking for too many non-linear components
## Result: Fatal Error
tmp <- frozenPcAuxOut1$copy()
miOut9 <- miWithPcAux(rawData = testData,
                      quarkData = tmp,
                      dropVars = "z2",
                      nImps = 5L,
                      nNonLinear = 42)

## Test:   Use defaults with no non-linear pcAux:
## Result: Successful execution
tmp <- frozenPcAuxOut12$copy()
miOut10 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       nImps = 5L)

## Test:   Request no non-linear components via counts
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut11 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       nNonLinear = 0)

## Test:   Request no non-linear components via variance explained
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut12 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       varExpNonLin = 0.0)

## Test:   Try requesting no linear components via counts
## Result: Fatal Error denoting the requirement for nLinear >= 1
tmp <- frozenPcAuxOut1$copy()
miOut13 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       nLinear = 0)

## Test:   Try requesting no linear components via variance explained
## Result: Fatal Error denoting the requirement for nLinear >= 1
tmp <- frozenPcAuxOut1$copy()
miOut14 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       varExpLin = 0.0)

## Test:   Specify nomVars different from quarkObj$nomVars
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut15 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       nomVars = "nom1",
                       dropVars = c("z2", "nom2"),
                       nImps = 5L)

## Test:   Specify ordVars different from quarkObj$ordVars
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut16 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       ordVars = "ord1",
                       dropVars = c("z2", "ord2"),
                       nImps = 5L)

## Test:   Specify idVars different from quarkObj$idVars
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut17 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       nomVars = "nom2",
                       idVars = c("id1", "id2", "nom1"),
                       dropVars = "z2",
                       nImps = 5L)

## Test: Trip initial check for distinct idVars
## Result: Fatal Error
tmp <- frozenPcAuxOut1$copy()
miOut18 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       idVars = c("id1", "id2", "nom1"),
                       dropVars = "z2",
                       nImps = 5L)

## Test:   Trip initial check for distinct dropVars
## Result: Fatal Error
tmp <- frozenPcAuxOut1$copy()
miOut19 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       idVars = c("id1", "id2"),
                       dropVars = c("z2", "z1", "y5", "ord1")
                       )

## Test:   Trip initial check for non-existant variables
## Result: Fatal Error
tmp <- frozenPcAuxOut1$copy()
miOut20 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       idVars = c("id1", "id2", "suzy"),
                       dropVars = c("z2", "z1", "y5")
                       )

## Test:   Request 'long' format for imputed data sets
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut21 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       completeFormat = "long")

## Test:   Request 'broad' format for imputed data sets
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut22 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       completeFormat = "broad")

## Test:   Request 'repeated' format for imputed data sets
## Result: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut23 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       completeFormat = "repeated")

## Test:   Call rom with no user-defined dropVars in quark():
## Result: Successful execution
tmp <- frozenPcAuxOut22$copy()
miOut24 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       nImps = 5L)

## Test:   Use miWithPcAux() with clean, normally distributed data:
## Result: Successful execution
tmp <- frozenPcAuxOut24$copy()
miOut25 <- miWithPcAux(rawData = testData3,
                       quarkData = tmp,
                       nImps = 5L)

## Test:   Use miWithPcAux() with clean,
##         normally distributed data with simMode = TRUE:
## Result: Successful execution
tmp <- frozenPcAuxOut25$copy()
miOut26 <- miWithPcAux(rawData = testData3,
                       quarkData = tmp,
                       nImps = 5L,
                       simMode = TRUE)

## Test:    Use parallel processing
## Results: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut27 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       useParallel = TRUE,
                       nProcess = 4)

## Test:   Time parallel vs. serial
## Result: Parallel is faster
tmp <- frozenPcAuxOut1$copy()
system.time(
    miOut27.2 <- miWithPcAux(rawData = testData,
                             quarkData = tmp,
                             dropVars = "z2",
                             nImps = 20L,
                             useParallel = TRUE,
                             nProcess = 20)
    )

tmp <- frozenPcAuxOut1$copy()
system.time(
    miOut27.3 <- miWithPcAux(rawData = testData,
                             quarkData = tmp,
                             dropVars = "z2",
                             nImps = 20L)
    )

## Test:    Use parallel processing with "long" output
## Results: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut28 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       useParallel = TRUE,
                       nProcess = 4,
                       completeFormat = "long")

## Test:    Use parallel processing with "broad" output
## Results: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut29 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       useParallel = TRUE,
                       nProcess = 4,
                       completeFormat = "broad")

## Test:    Use parallel processing with "repeated" output
## Results: Successful execution
tmp <- frozenPcAuxOut1$copy()
miOut30 <- miWithPcAux(rawData = testData,
                       quarkData = tmp,
                       dropVars = "z2",
                       nImps = 5L,
                       useParallel = TRUE,
                       nProcess = 4,
                       completeFormat = "repeated")
