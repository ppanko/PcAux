\name{getImpData}
\alias{getImpData}
\title{
  Extract multiply imputed datasets from a PcAuxData object.
}
\description{
  This is a simple wrapper function that extracts the completed,
  multiply imputed data sets from a fitted PcAuxData object produced by
  running the \code{miWithPcAux} function.
}
\usage{
getImpData(pcAuxData)
}
\arguments{
  \item{pcAuxData}{
    A fitted object of class \code{PcAuxData} produced as output of the
    \code{miWithPcAux} function.
  }
}
\value{
  A set of multiply imputed data sets. The format of these data sets is
  defined by the \code{compFormat} value in \code{pcAuxData}. See
  \code{\link{miWithPcAux}} for more information.
}
\author{
  Kyle M. Lang
}
\seealso{
  \code{\link{miWithPcAux}}, \code{\link{createPcAux}}
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

## Extract a list of imputed data sets:
impList <- getImpData(pcAuxData = miOut)
}