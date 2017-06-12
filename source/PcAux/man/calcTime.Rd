\name{calcTime}
\alias{calcTime}
\title{
  Calculates the time between PcAux processes.
}
\description{
  This function records the system time for the PcAux functions at set intervals 
  to help determine issues with lengthy runs. calcTime allows the user
  to extract timing information for specific functions of PcAux
  individually. 
}
\usage{
calcTime(pcAuxData, what)
}
\arguments{
  \item{pcAuxData}{
    A fitted object of class \code{PcAuxData} produced as output of
    \code{prepData}, \code{createPcAux}, or \code{miWithPcAux} functions.
  }
  \item{what}{
    A character vector indicating the name of a function for which to
    extract status information. abbreviates the functions
    \code{prepData}, \code{createPcAux}, and \code{miWithPcAux} as
    "prep", "create", and "mi", respectively. 
  }
}
\value{
  A named vector with an entry per interval.
}
\author{
  Pavel Panko
}
\seealso{
  \code{\link{writeStatus}}
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

## Conduct MI with the pcAux:
miOut <- miWithPcAux(rawData   = iris2,
                     pcAuxData = pcAuxOut,
                     nImps     = 5)

## Extract timing infromation:
timeInfo <- calcTime(pcAuxData = miOut, what = "mi")
}