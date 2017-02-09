\name{miWithPcAux}
\alias{miWithPcAux}
\title{
  Create multiple imputations using the PcAux produced by \code{createPcAux}.
}
\description{
  Create multiple imputations with the \pkg{mice} package using the
  principal component auxiliary variable scores produced by
  \code{createPcAux} as the predictors in the imputation model.
}
\usage{
miWithPcAux(rawData,
            quarkData,
            nImps          = 100L,
            nomVars        = NULL,
            ordVars        = NULL,
            idVars         = NULL,
            dropVars       = "useExtant",
            nLinear        = NULL,
            nNonLinear     = NULL,
            varExpLin      = NULL,
            varExpNonLin   = NULL,
            completeFormat = "list",
            mySeed         = 235711L,
            simMode        = FALSE,
            forcePmm       = FALSE,
            useParallel    = FALSE,
            nProcess       = 1L,
            verbose        = !simMode,
            control)
}
\arguments{
  \item{rawData}{
    A data frame containing the incomplete data for which to create
    the multiple imputations.
  }
  \item{quarkData}{
    An object of class \code{QuarkData} produced by a run of
    \code{createPcAux}.
  }
  \item{nImps}{
    An integer giving the number of imputations to create. Defaults to
    \code{nImps = 100L}.
  }
  \item{nomVars}{
    An optional character vector containing names of any nominal
    variables (i.e., unordered factors) that exist in \code{rawData}. If
    unspecified, any \code{nomVars} defined in \code{quarkData} will be
    used.
  }
  \item{ordVars}{
    An optional character vector containing names of any ordinal
    variables (i.e., ordered factors) that exist in \code{rawData}. If
    unspecified, any \code{ordVars} defined in \code{quarkData} will be
    used.
  }
  \item{idVars}{
    An optional character vector containing names of any ID variables
    that exist in \code{rawData}. Any columns flagged as ID variables
    should not be represented in \code{nomVars}, \code{ordVars},
    \code{dropVars}, or \code{groupVars}. If unspecified, any
    \code{idVars} defined in \code{quarkData} will be used.
  }
  \item{dropVars}{
    An optional character vector containing names of any nuisance
    variables that should be excluded from the imputation process. If
    unspecified, the default value of \code{dropVars = "useExtant"}
    causes any user-defined \code{dropVars} defined in \code{quarkData}
    to be used.
  }
  \item{nLinear}{
    An integer giving the number of linear principal component auxiliary
    scores to use as predictors in the imputation model. If unspecified,
    all linear principal component auxiliary scores contained in
    \code{quarkData} will be used. 
  }
  \item{nNonLinear}{
    An integer giving the number of non-linear principal component
    auxiliary scores to use as predictors in the imputation model. If
    unspecified, all non-linear principal component auxiliary scores
    contained in \code{quarkData} will be used. 
  }
  \item{varExpLin}{
    An optional real number giving the proportion of variance to be
    explained by the linear principal component auxiliary scores (see
    'Details' for a description of the implementation). This argument
    overrides \code{nLinear}.  If unspecified, the \code{nLinear}
    argument will define the number of linear principal component scores
    used. 
  }
  \item{varExpNonLin}{
    An optional real number giving the proportion of variance to be
    explained by the non-linear principal component auxiliary
    scores (see 'Details' for a description of the implementation). This
    argument overrides \code{nNonLinear}. If unspecified, the
    \code{nNonLinear} argument will define the number of non-linear 
    principal component scores used.
  }
  \item{completeFormat}{
    The format in which the multiply-imputed data sets are
    returned. Valid arguments are "list", which returns a list of length
    \code{nImps} with each entry containing one imputed data set,
    "long", "broad", and "repeated". The latter three options are passed
    directly to the \code{action} argument of the \code{mice::complete}
    function. See the documentation for \code{mice::complete} for more
    details on the behavior of the "long", "broad", and "repeated"
    options. Defaults to \code{completeFormat = "list"}. 
  }
  \item{mySeed}{
    An integer used to seed the random number generator used by the
    imputation algorithm. Defaults to \code{mySeed = 235711L}.
  }
  \item{simMode}{
    A logical switch turning 'Simulation Mode' on or off. In Simulation
    Mode all of the automatic data checks will be suppressed. This mode
    is intended for use when \code{miWithPcAux} is being called as part of a
    Monte Carlo simulation study in which the data properties are
    well-known by the user. This mode should not be used for
    'real-world' data analysis. Defaults to \code{simMode = FALSE}.
  }
  \item{forcePmm}{
    A logical flag indicating whether or not the imputation should use
    predictive mean matching as the elementary imputation method for
    (almost) all variables. If \code{forcePmm == FALSE}, the elementary
    imputation methods are chosen to match each variable's declared
    type. When \code{forcePmm == TRUE}, nominal variables are still
    imputed with GLM-based methods appropriate for their declared types,
    but all other variables are imputed with PMM. Defaults to
    \code{forcePmm = FALSE}.
  }
  \item{useParallel}{
    A logical switch indicating if the multiple imputation should be
    executed using parallel processing. When set to \code{TRUE}, the
    imputation job will be executed in \code{nProcess} parallel jobs
    using the \code{parLapply} function from the \pkg{parallel}
    package. Defaults to \code{useParallel = FALSE}.
  }
  \item{nProcess}{
    An integer that gives the number of parallel processes to use when
    \code{useParallel == TRUE}. Must be less than or equal to the number
    of available logical processor cores. Defaults to \code{nProcess =
      1L}.
  }
  \item{verbose}{
    A logical switch indicating whether output should be printed to the
    screen. Warnings are always printed, regardless of the value
    assigned to \code{verbose}. Defaults to \code{verbose = !simMode}.
  }
  \item{control}{
    An optional list of control parameters (see 'Details').
  }
}
\details{
  When non-zero values are provided for \code{varExpLin} and/or
  \code{varExpNonLin}, the number of linear and/or non-linear principal
  component auxiliary scores used as predictors in the imputation model
  is taken to be the minimum number needed to explain \emph{at least} as
  much variance in \code{rawData} as the proportions given by
  \code{varExpLin} and/or \code{varExpNonLin}. This means that the
  actual proportions of variance explain by the selected number of
  components will usually be slightly higher than the threshold
  requested, because the existence of a set of principal component
  scores that explain exactly the requested proportion of variance is
  unlikely.

  The \code{control} argument is a key-paired list with the following
  possible entries:
  \itemize{
    \item{miceRidge: }{
      Value of the ridge penalty parameter used to stabilize the
      imputation models used by mice(). Defaults to \code{miceRidge =
	1e-5}.
    }
    \item{minRespCount: }{
      The minimum number of observations allowed on each variable without
      triggering a warning. Defaults to \code{floor(0.05 * nrow(rawData))}.
    }
    \item{maxNetWts: }{
      The maximum number of network weights used by nnet() to fit the
      polytimous regression models used to impute nominal variables with
      mice(). Defaults to \code{maxNetWts = 10000L}.
    }
    \item{nomMaxLev: }{
      The maximum number of response levels for nominal variables that
      won't trigger a warning. Defaults to \code{nomMaxLev = 10L}.
    }
    \item{ordMaxLev: }{
      The maximum number of response levels for ordinal variables that
      won't trigger a warning. Defaults to \code{ordMaxLev = 10L}.
    }
    \item{conMinLev: }{
      The minimum number of unique responses for continuous variables that
      won't trigger a warning. Defaults to \code{minConLev = 10L}.
    }
  }
  Note that the behavior of the \code{forcePmm} argument changed between
  versions 0.6.1 and 0.6.2 of quark. In earlier releases (<0.6.2)
  setting \code{forcePmm = TRUE} caused all variables to be imputed with
  PMM. Later releases do not use PMM for nominal variable since the
  underlying matching rule is inappropriate for unordered variables.  
}
\value{
  A Reference Class object of class QuarkData with all of the fields
  from the object provided to the \code{quarkData} argument preserved,
  new fields for each of the \code{miWithPcAux} function's arguments and
  the following modified or additional fields:
  \itemize{
    \item{call: }{
      A list containing the matched function call to \code{miWithPcAux}.
    }
    \item{miDatasets: }{
      The completed, multiply imputed data sets. The structure of this
      field's contents is dictated by the \code{compFormat} argument to
      \code{miWithPcAux}.
    }
    \item{miceObject: }{
      The \code{mids} object returned by \pkg{mice} in the process of
      creating the multiple imputations of \code{rawData}.
    }
    \item{nComps: }{
      An integer vector of length 2 that contains the number of linear
      and non-linear, respectively, principal component auxiliary
      variable scores used as predictors in the multiple imputation
      models.
    }
    \item{typeVec: }{
      A character vector giving the types assigned to each variable in
      \code{rawData}. 
    }
    \item{methVec: }{
      A character vector giving the elementary imputation methods used
      by \pkg{mice}.
    }
    \item{respCounts: }{
      An integer vector giving the variable-wise response counts for
      \code{rawData}. 
    }
    \item{initialPm: }{
      A numeric vector giving the initial, variable-wise percents
      missing for \code{rawData}, before any treatment.
    }
    \item{dropVars: }{
      A two-column character matrix. The first column contains the names
      of all variables that were excluded from the imputation process
      (these variables appear in their original, incomplete, form in the
      multiply imputed data sets). The second column contains the
      reason that the corresponding variable was excluded.
    }
    \item{probNoms: }{
      A character vector giving the variable names for any nominal
      variables with more levels than \code{control$nomMaxLev}.
    }
    \item{probOrds: }{
      A character vector giving the variable names for any ordinal
      variables with more levels than \code{control$ordMaxLev}.
    }
    \item{probCons: }{
      A character vector giving the variable names for any continuous
      variables with fewer levels than \code{control$conMinLev}.
    }
    \item{levelVec: }{
      An integer vector giving the number of unique, non-missing, levels
      for each column of \code{rawData}.
    }
    \item{highPmVars: }{
      A character vector containing the names of variables with fewer
      observed responses than \code{control$minRespCount}.
    }
    \item{emptyVars: }{
      A character vector giving the names of empty columns in
      \code{rawData}.
    }
    \item{constants: }{
      A character vector giving the names of constant columns in
      \code{rawData}.
    }
  }
}
\author{
  Kyle M. Lang & Stephen Chesnut
}
\seealso{
  \code{\link{createPcAux}}
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
pcAuxOut <- createPcAux(quarkData = cleanData,
                        nComps    = c(3, 2)
                        )

## Conduct MI with the pcAux:
miOut <- miWithPcAux(rawData   = iris2,
                     quarkData = pcAuxOut,
                     nImps     = 5)
}