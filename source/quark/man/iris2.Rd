\name{iris2}
\alias{iris2}
\docType{data}
\title{
  A modified version of the Fisher/Anderson iris data. 
}
\description{
  This is a slight modification of the famous Fisher/Anderson iris
  data. I've binned petal width and added an ID and junk variable to
  demonstrate the usage of \pkg{package:quark} more effectively.
}
\usage{data("iris2")}
\format{
  A data frame with 150 observations on the following 7 variables
  describing the characteristics of a sample of three species of iris.
  \describe{
    \item{\code{ID}}{
      A numeric vector of IDs
    }
    \item{\code{Sepal.Length}}{
      A numeric vector of sepal lengths
    }
    \item{\code{Sepal.Width}}{
      A numeric vector of sepal widths
    }
    \item{\code{Petal.Length}}{
      A numeric vector of petal lengths
    }
    \item{\code{Petal.Width}}{
      An ordered factor with levels \code{1} < \code{2} < \code{3} <
      \code{4} < \code{5} giving a categorized measure of petal width
    }
    \item{\code{Species}}{
      A factor with levels \code{setosa} \code{versicolor}
      \code{virginica} giving the iris' species
    }
    \item{\code{Junk}}{
      A constant nuisance factor with levels \code{badVar}
    }
  }
}
\source{
  Fisher, R. A. (1936) The use of multiple measurements in taxonomic
  problems. Annals of Eugenics, 7, 179-188.
  
  The data were collected by:
  Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin
  of the American Iris Society, 59, 2-5.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The new s
  language. Wadsworth & Brooks/Cole.
}
\examples{
data(iris2)
}
\keyword{datasets}
