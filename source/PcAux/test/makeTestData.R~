### Title:    Quark Test Data Simulation
### Author:   Kyle M. Lang
### Created:  2015-JUL-27
### Modified: 2016-FEB-18
### Note:     Source this code to get a testing data set for Quark development.

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


library(mvtnorm)

set.seed(235711)

samSize <- 500

mu <- sample(c(1 : 5), 15, replace = TRUE)
sigma <- matrix(0.3, 15, 15)
diag(sigma) <- 1.0
dat1 <- rmvnorm(samSize, mu, sigma)

Y <- dat1[ , 1 : 5]
W <- dat1[ , 6 : 10]
Z <- dat1[ , 11 : 15]

colnames(W) <- paste0("w", c(1 : 5))
colnames(Y) <- paste0("y", c(1 : 5))

Z <- data.frame(
    lapply(c(1 : ncol(Z)),
           FUN =
               function(x, data, breaks) {
                   tmpBreak <- breaks[x]
                   cut(data[ , x],
                       breaks = tmpBreak,
                       labels = c(1 : tmpBreak)
                       )
               },
           breaks = c(2, 4, 5, 7, 12),
           data = Z) 
)

colnames(Z) <- c("nom1", "nom2", "ord1", "ord2", "bigCat")

## x1 is collinear with y5:
x1 <- 2 * Y[ , "y5"]

## x2 is nearly collinear with y4:
tmpSd <- 0.25 * sd(2 * Y[ , "y4"])
x2 <- 2 * Y[ , "y4"] + rnorm(samSize, 0, tmpSd)

## x3 is constant:
x3 <- rep(42, nrow(Y))

## One numeric and one factor ID:
id1 <- c(1 : nrow(Y))
id2 <- as.factor(paste0("bob", c(1 : nrow(Y))))

tmpData <- data.frame(id1, id2, Y, Z, x1, x2, x3)

## Impose MCAR missing data:
missFlag <- matrix(
    as.logical(
        rbinom(n = prod(dim(tmpData)), size = 1, prob = 0.2)
    ),
    ncol = ncol(tmpData)
)

tmpData[missFlag] <- NA

## Empty variable:
z1 <- NA

## High PM variable:
tmp <- rep(NA, ceiling(0.96 * nrow(tmpData)))
z2 <- c(tmp, rnorm(nrow(tmpData) - length(tmp)))

testData <- data.frame(tmpData, z1, z2, W)



