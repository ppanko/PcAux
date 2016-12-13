\name{getImpData}
\alias{getImpData}
\title{
  Extract multiply imputed datasets from a QuarkData object.
}
\description{
  This is a simple wrapper function that extracts the completed,
  multiply imputed data sets from a fitted QuarkData object produced by
  running the \code{miWithPcAux} function.
}
\usage{
getImpData(quarkData)
}
\arguments{
  \item{quarkData}{
    A fitted object of class \code{QuarkData} produced as output of the
    \code{miWithPcAux} function.
  }
}
\value{
  A set of multiply imputed data sets. The format of these data sets is
  defined by the \code{completeFormat} value in \code{quarkData}. See
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
cleanData <- prepData(rawData = iris2,
                      nomVars = "Species",
                      ordVars = "Petal.Width",
                      idVars = "ID",
                      dropVars = "Junk",
                      groupVars = "Species")

## Create principal component auxiliary variables:
pcAuxOut <- createPcAux(quarkData = cleanData,
                        nComps = c(3, 2)
                        )

## Conduct MI with the pcAux:
miOut <- miWithPcAux(rawData = iris2,
                     quarkData = pcAuxOut,
                     nImps = 5)

## Extract a list of imputed data sets:
impList <- getImpData(quarkData = miOut)
}