\name{makePredMatrix}
\alias{makePredMatrix}
\title{
  Make a predictor matrix for use with \pkg{mice}.
}
\description{
  Make a predictor matrix for use with \pkg{mice} that correctly
  specifies the auxiliary principal component scores produced by
  \code{createPcAux} as the sole predictors in the imputation model.
}
\usage{
makePredMatrix(mergedData,
               nLinear      = NULL,
               nNonLinear   = NULL,
               useQuickPred = FALSE,
               minCor       = NULL)
}
\arguments{
  \item{mergedData}{
    A data frame, such as one returned by \code{PcAux::mergePcAux},
    containing the incomplete variables to be imputed and the principal
    component auxiliary variable scores.
  }
  \item{nLinear}{
    The number of linear principal component auxiliaries to use as
    predictors in the imputation model. If not specified, all linear PcAux
    scores contained in \code{mergedData} will be used.
  }
  \item{nNonLinear}{
    The number of non-linear principal component auxiliaries to use as
    predictors in the imputation model. If not specified, all non-linear
    PcAux scores contained in \code{mergedData} will be used.
  }
  \item{useQuickPred}{
    A logical flag indicator whether or not to subject the final predictor
    matrix to the \code{mice::quickpred} prodecure. Specifying
    \code{useQuickPred = TRUE} will screen the PcAux scores used as
    predictors and only employ those that correlate with the imputation
    target more strongly than the threshold defined by the
    \code{minCor} option.
  }
  \item{minCor}{
    The minimum bivariate correlation used by the \code{mice::quickpred}
    function to screen the PcAux candidates when the
    \code{useQuickPred} option is specified.
  }
}
\value{
  A pattern matrix with dimensions: \code{c(ncol(mergedData),
    ncol(mergedData))} that can be supplied to the
  \code{predictorMatrix} argument of \pkg{mice}.
}
\author{
  Kyle M. Lang
}
\seealso{
  \code{\link{miWithPcAux}}
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
pcAuxOut <- createPcAux(pcAuxData = cleanData, nComps = c(3, 0))

## Merge the PC auxiliaries with the original data:
outData <- mergePcAux(pcAuxData = pcAuxOut, rawData = iris2)

## Create a predictor matrix:
predMat <- makePredMatrix(mergedData = outData)
}