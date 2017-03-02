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
mergePcAux(quarkData, rawData, nComps = NULL, verbose = TRUE)
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
  \item{nComps}{
    A two-element vector giving the number of linear and
    nonlinear, respectively, component scores to extract. See the
    \emph{Details} section for more information. When not specified, all
    component scores that exist in \code{quarkData} are used.
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
  
  The number of component scores requested via the \code{nComps}
  argument can be defined in two ways: as simple counts, or in terms of
  the proportion of variance in the data explained by the set of
  extracted components. When specifying \code{nComps}, integer
  arguments are interpreted as counts of components; real-valued
  arguments between 0 and 1.0 are interpreted as proportions of
  variance explained, and the special character token "max" indicates
  the smallest number of component scores that explain 100\% of the
  variance in the raw data. An error is returned when more components
  are requested than exist in \code{quarkData}.  
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