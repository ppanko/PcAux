\name{getPredMat}
\alias{getPredMat}
\title{
  Extract the predictor matrix used by mice from a PcAuxData object.
}
\description{
  This function that extracts the predictor matrix from a mids object
  inside of a PcAuxData object.
}
\usage{
getPredMat(pcAuxData)
}
\arguments{
  \item{pcAuxData}{
    A fitted object of class \code{PcAuxData} produced as output of the
    \code{prepData}, \code{createPcAux}, or \code{miWithPcAux} function.
  }
}
\value{
  A matrix indicating the predictor variables used by mice. 
}
\author{
  Pavel Panko
}
\seealso{
  \code{\link{createPcAux}}, \code{\link{miWithPcAux}}
}
\examples{
## Load the data:
data(iris2)

## Prepare the data:
cleanData <- prepData(rawData   = iris2,
                      nomVars   = "Species",
                      ordVars   = "Petal.Width",
                      idVars    = "ID",
                      dropVars  = "Junk",
                      groupVars = "Species")

## Create principal component auxiliary variables:
pcAuxOut <- createPcAux(pcAuxData    = cleanData,
                        nComps       = c(3, 2),
                        interactType = 2)

## Retrieve predictor matrix
getPredMat(pcAuxOut)
}
