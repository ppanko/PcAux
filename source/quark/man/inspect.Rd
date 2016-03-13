\name{inspect}
\alias{inspect}
\title{
  Access fields of a QuarkData object.
}
\description{
  Provide S3/S4-like access to fields of a QuarkData Reference Class
  object.
}
\usage{
inspect(object, what)
}
\arguments{
  \item{object}{
    An initialized RC object of class QuarkData.
  }
  \item{what}{
    A character string naming the field to access in \code{object}.
  }
}
\value{
  The current value stored in the \code{what} field of \code{object}.
}
\author{
  Kyle M. Lang
}
\examples{
## Load data:
data(iris2)

## Prepare the data:
newData <- prepData(rawData = iris2,
                    nomVars = "Species",
                    ordVars = "Petal.Width",
                    idVars = "ID",
                    dropVars = "Junk",
                    groupVars = "Species")

## Pull the 'data' field from 'newData':
inspect(object = newData, what = "data")
}