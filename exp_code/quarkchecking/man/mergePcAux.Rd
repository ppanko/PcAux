\name{mergePcAux}
\alias{mergePcAux}
\title{
  Merge Principal Component Auxiliaries with the raw data from which
  they were constructed.
}
\description{
  Merge Auxiliary Principal Component scores produced by
  \code{createPcAux} with a data frame containing the raw data from
  which the component scores were constructed.
}
\usage{
mergePcAux(quarkData,
           rawData,
           nLinear = NULL,
           nNonLinear = NULL,
           varExpLin = NULL,
           varExpNonLin = NULL,
           verbose = TRUE)
}
\arguments{
  \item{quarkData}{
    An object of class \emph{QuarkData} produced by a call to
    \code{createPcAux}.
  }
  \item{rawData}{
    A data frame containing the raw data used to run
    \code{createPcAux}. 
  }
  \item{nLinear}{
    An optional integer giving the number of linear principal component
    auxiliary scores to merge onto \code{rawData}. If unspecified, all
    linear principal component scores that exist in \code{quarkData} are
    used.
  }
  \item{nNonLinear}{
    An optional integer giving the number of non-linear principal
    component auxiliary scores to merge onto \code{rawData}. If
    unspecified, all non-linear principal component scores that exist in
    \code{quarkData} are used. 
  }
  \item{varExpLin}{
    An optional real number giving the proportion of variance in
    \code{rawData} to be explained by the set of linear principal
    component auxiliary scores merged onto \code{rawData} (see 'Details'
    for a description of the implementation). If unspecified, the
    \code{nLinear} argument defines the number of linear component
    scores used. 
  }
  \item{varExpNonLin}{
    An optional real number giving the proportion of variance in
    \code{rawData} to be explained by the set of non-linear principal
    component auxiliary scores merged onto \code{rawData} (see 'Details'
    for a description of the implementation). If unspecified, the
    \code{nNonLinear} argument defines the number of non-linear
    component scores used. 
  }
  \item{verbose}{
    A logical flag indicating whether verbose output should be printed
    to stdout. Defaults to \code{verbose = TRUE}.
    }  
}
\details{
  This function will attempt to use the ID variables defined in Quark's
  \code{idVars} argument to align rows for merging. If these ID
  variables are not suitable (i.e., because they don't exist in the raw
  data or they're not unique row-identifiers), the merging will be
  accomplished via naive column-binding.

  When non-zero values are provided for \code{varExpLin} and/or
  \code{varExpNonLin}, the number of linear and/or non-linear principal
  component auxiliary scores that are merged onto \code{rawData} is
  taken to be the minimum number needed to explain \emph{at least} as 
  much variance in \code{rawData} as the proportions given by
  \code{varExpLin} and/or \code{varExpNonLin}. This means that the
  actual proportions of variance explain by the selected number of
  components will usually be slightly higher than the threshold
  requested, because the existence of a set of principal component
  scores that explain exactly the requested proportion of variance is
  unlikely. 
}
\value{
  A data frame with (a subset of) the principal component auxiliary
  scores from \code{quarkData$pcAux} merged onto the end of the raw
  data. 
}
\author{
  Kyle M. Lang
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

## Merge the PC auxiliaries with the original data:
outData <- mergePcAux(quarkData = pcAuxOut, rawData = iris2)
}